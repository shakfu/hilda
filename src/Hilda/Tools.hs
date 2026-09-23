-- | The four built-in tools: @read@, @write@, @edit@, @bash@.
--
-- A tool is a record of data plus one effectful function, so the set of
-- tools is an ordinary list that callers filter and extend.
module Hilda.Tools
  ( Effect (..)
  , Tool (..)
  , toolSpec
  , builtinTools
  , readTool
  , writeTool
  , editTool
  , bashTool
    -- * Pure helpers
  , numberLines
  , applyEdit
  , truncateMiddle
  ) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar
import Control.Exception (onException, throwIO)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO
import System.Posix.Signals (signalProcessGroup, sigKILL)
import System.Process
import System.Timeout (timeout)

-- | What a tool can do to the machine. Policies decide on this, not names.
data Effect = Observe | Mutate | Execute
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Tool = Tool
  { toolName        :: Text
  , toolDescription :: Text
  , toolParams      :: Value -- ^ JSON Schema of the arguments.
  , toolEffect      :: Effect
  , toolRun         :: Value -> IO (Either Text Text)
  }

-- | Function-calling spec sent to the model.
toolSpec :: Tool -> Value
toolSpec t =
  object
    [ "type" .= ("function" :: Text)
    , "function"
        .= object
          ["name" .= toolName t, "description" .= toolDescription t, "parameters" .= toolParams t]
    ]

builtinTools :: [Tool]
builtinTools = [readTool, writeTool, editTool, bashTool]

-- | Object schema from (name, JSON type, description) triples.
schema :: [(Key, Text, Text)] -> [Key] -> Value
schema props required =
  object
    [ "type" .= ("object" :: Text)
    , "properties" .= object [k .= object ["type" .= ty, "description" .= d] | (k, ty, d) <- props]
    , "required" .= required
    ]

-- | Parse arguments, then run; a parse failure becomes the tool's error.
withArgs :: (Object -> Parser a) -> (a -> IO (Either Text Text)) -> Value -> IO (Either Text Text)
withArgs p k v = either (pure . Left . T.pack) k (parseEither (withObject "arguments" p) v)

readTool :: Tool
readTool =
  Tool
    { toolName = "read"
    , toolDescription = "Read a UTF-8 text file. Returns lines prefixed with their line number."
    , toolParams =
        schema
          [ ("path", "string", "File path")
          , ("offset", "integer", "First line to return, 1-based (default 1)")
          , ("limit", "integer", "Maximum number of lines (default 2000)")
          ]
          ["path"]
    , toolEffect = Observe
    , toolRun = withArgs (\o -> (,,) <$> o .: "path" <*> o .:? "offset" .!= 1 <*> o .:? "limit" .!= 2000) $
        \(path, offset, limit) -> do
          bytes <- BS.readFile path
          pure $
            if BS.elem 0 bytes
              then Left (T.pack path <> " looks binary (contains NUL bytes)")
              else Right (numberLines offset limit (decodeUtf8Lenient bytes))
    }

writeTool :: Tool
writeTool =
  Tool
    { toolName = "write"
    , toolDescription = "Create or overwrite a file with the given content. Creates parent directories."
    , toolParams = schema [("path", "string", "File path"), ("content", "string", "Full file content")] ["path", "content"]
    , toolEffect = Mutate
    , toolRun = withArgs (\o -> (,) <$> o .: "path" <*> o .: "content") $ \(path, content) -> do
        let bytes = encodeUtf8 content
        atomicWrite path bytes
        pure (Right ("wrote " <> tshow (BS.length bytes) <> " bytes to " <> T.pack path))
    }

editTool :: Tool
editTool =
  Tool
    { toolName = "edit"
    , toolDescription =
        "Replace exact text in a file. old_string must match exactly once unless replace_all is true."
    , toolParams =
        schema
          [ ("path", "string", "File path")
          , ("old_string", "string", "Exact text to replace")
          , ("new_string", "string", "Replacement text")
          , ("replace_all", "boolean", "Replace every occurrence (default false)")
          ]
          ["path", "old_string", "new_string"]
    , toolEffect = Mutate
    , toolRun =
        withArgs
          (\o -> (,,,) <$> o .: "path" <*> o .: "old_string" <*> o .: "new_string" <*> o .:? "replace_all" .!= False)
          $ \(path, old, new, replaceAll) -> do
            src <- decodeUtf8Lenient <$> BS.readFile path
            case applyEdit old new replaceAll src of
              Left err -> pure (Left err)
              Right out -> do
                atomicWrite path (encodeUtf8 out)
                pure (Right ("edited " <> T.pack path))
    }

bashTool :: Tool
bashTool =
  Tool
    { toolName = "bash"
    , toolDescription =
        "Run a command with bash -c in the current directory. Returns combined stdout/stderr and the exit code."
    , toolParams =
        schema
          [("command", "string", "Shell command"), ("timeout", "integer", "Seconds before the command is killed (default 120)")]
          ["command"]
    , toolEffect = Execute
    , toolRun = withArgs (\o -> (,) <$> o .: "command" <*> o .:? "timeout" .!= 120) $ \(cmd, secs) ->
        runShell (max 1 secs) (T.unpack cmd) >>= \case
          Nothing -> pure (Left ("timed out after " <> tshow secs <> "s"))
          Just (code, out) -> pure (Right (T.stripEnd out <> "\n[exit code " <> tshow (exitInt code) <> "]"))
    }
  where
    exitInt ExitSuccess     = 0
    exitInt (ExitFailure n) = n

-- | Run a shell command in its own process group with stdout and stderr on
-- one pipe. On timeout or exception the whole group gets SIGKILL, so
-- pipelines and background children die with it.
runShell :: Int -> String -> IO (Maybe (ExitCode, Text))
runShell secs cmd = do
  (readEnd, writeEnd) <- createPipe
  devNull <- openFile "/dev/null" ReadMode
  let cp =
        (proc "bash" ["-c", cmd])
          { std_in = UseHandle devNull
          , std_out = UseHandle writeEnd
          , std_err = UseHandle writeEnd
          , create_group = True
          }
  withCreateProcess cp $ \_ _ _ ph -> do
    let kill = getPid ph >>= mapM_ (signalProcessGroup sigKILL)
    done <- newEmptyMVar
    -- The reader closes its own end: closing it here could block on the
    -- handle lock while the reader waits for an EOF that never comes.
    _ <- forkFinally (readCapped captureLimit readEnd) (\r -> hClose readEnd >> putMVar done r)
    flip onException kill $
      timeout (secs * 1000000) (takeMVar done) >>= \case
        Nothing -> kill >> pure Nothing
        Just (Left e) -> kill >> throwIO e
        Just (Right out) -> do
          code <- waitForProcess ph
          pure (Just (code, decodeUtf8Lenient out))

captureLimit :: Int
captureLimit = 1024 * 1024

-- | Read to EOF, keeping at most @limit@ bytes and discarding the rest so
-- the writer never blocks on a full pipe.
readCapped :: Int -> Handle -> IO BS.ByteString
readCapped limit h = go 0 []
  where
    go n acc = do
      chunk <- BS.hGetSome h 65536
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else
          let keep = BS.take (limit - n) chunk
           in go (n + BS.length keep) (if BS.null keep then acc else keep : acc)

-- | Write via a temporary file and rename, so a crash never leaves a
-- truncated file. Resolves symlinks first and keeps existing permissions.
atomicWrite :: FilePath -> BS.ByteString -> IO ()
atomicWrite path bytes = do
  target <- canonicalizePath path
  createDirectoryIfMissing True (takeDirectory target)
  existed <- doesFileExist target
  perms <- if existed then Just <$> getPermissions target else pure Nothing
  let tmp = target <> ".hilda-tmp"
  BS.writeFile tmp bytes
  mapM_ (setPermissions tmp) perms
  renameFile tmp target

-- | Number lines from @offset@ (1-based), returning at most @limit@ lines.
numberLines :: Int -> Int -> Text -> Text
numberLines offset limit src
  | null picked = "(no lines in range; file has " <> tshow (length ls) <> " lines)"
  | otherwise = T.unlines [tshow n <> "\t" <> l | (n, l) <- picked]
  where
    ls = T.lines src
    picked = take (max 0 limit) (drop (max 0 (offset - 1)) (zip [1 :: Int ..] ls))

-- | Replace @old@ with @new@. Refuses ambiguous matches unless @replaceAll@.
applyEdit :: Text -> Text -> Bool -> Text -> Either Text Text
applyEdit old new replaceAll src
  | T.null old = Left "old_string is empty"
  | old == new = Left "old_string and new_string are identical"
  | otherwise = case T.count old src of
      0 -> Left "old_string not found"
      1 -> Right (T.replace old new src)
      n
        | replaceAll -> Right (T.replace old new src)
        | otherwise ->
            Left ("old_string matches " <> tshow n <> " times; add context or set replace_all")

-- | Keep the head and tail of text longer than @limit@ characters.
truncateMiddle :: Int -> Text -> Text
truncateMiddle limit t
  | T.length t <= limit = t
  | otherwise =
      T.take half t <> "\n[... " <> tshow (T.length t - 2 * half) <> " characters omitted ...]\n" <> T.takeEnd half t
  where
    half = limit `div` 2

tshow :: Show a => a -> Text
tshow = T.pack . show
