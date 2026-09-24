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
  , atomicWrite
  , resultLimit
  , editLimit
    -- * Pure helpers
  , numberLines
  , applyEdit
  , truncateMiddle
  ) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar
import Control.Exception (IOException, evaluate, onException, throwIO, try)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, decodeUtf8', encodeUtf8)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName)
import System.IO
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Posix.Signals (signalProcessGroup, sigKILL)
import System.Process
import System.Timeout (timeout)

-- | What a tool can do to the machine. Policies decide on this, not names.
data Effect = Observe | Mutate | Execute
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A tool the model can call.
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

-- | @read@, @write@, @edit@ and @bash@, in the order offered to the model.
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

-- | Read a text file with line numbers, paged below 'resultLimit'. Refuses binary files.
readTool :: Tool
readTool =
  Tool
    { toolName = "read"
    , toolDescription =
        "Read a UTF-8 text file. Returns lines prefixed with their line number. "
          <> "Long output ends with the offset to continue from."
    , toolParams =
        schema
          [ ("path", "string", "File path")
          , ("offset", "integer", "First line to return, 1-based (default 1)")
          , ("limit", "integer", "Maximum number of lines (default 2000)")
          ]
          ["path"]
    , toolEffect = Observe
    , toolRun = withArgs (\o -> (,,) <$> o .: "path" <*> o .:? "offset" .!= 1 <*> o .:? "limit" .!= 2000) $
        \(path, offset, limit) ->
          -- Lazy read, forced inside the bracket: memory grows with the
          -- lines returned, not the file size.
          withBinaryFile path ReadMode $ \h -> do
            bytes <- BL.hGetContents h
            if BL.elem 0 (BL.take 8192 bytes)
              then pure (Left (T.pack path <> " looks binary (NUL byte in the first 8 KiB)"))
              else do
                let out = numberLines readBudget offset limit (map (decodeUtf8Lenient . BL.toStrict) (BL8.lines bytes))
                Right out <$ evaluate (T.length out)
    }

-- | Create or replace a file through 'atomicWrite'.
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

-- | Exact replacement through 'applyEdit'. Refuses non-UTF-8 files and files over 'editLimit'.
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
            size <- getFileSize path
            if size > fromIntegral editLimit
              then pure (Left (T.pack path <> " has " <> tshow size <> " bytes; edit is limited to " <> tshow editLimit <> ", use bash"))
              else do
                bytes <- BS.readFile path
                -- A lenient decode would rewrite every invalid byte as U+FFFD.
                case either (const (Left (T.pack path <> " is not valid UTF-8; use bash"))) Right (decodeUtf8' bytes) >>= applyEdit old new replaceAll of
                  Left err -> pure (Left err)
                  Right out -> do
                    atomicWrite path (encodeUtf8 out)
                    pure (Right ("edited " <> T.pack path))
    }

-- | Run @bash -c@. The default timeout is 120 s; on timeout the process group is killed.
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
    -- Kill the group, then reap bash here rather than leave it to
    -- withCreateProcess's background cleanup.
    let kill = do
          getPid ph >>= mapM_ (signalProcessGroup sigKILL)
          void (try @IOException (waitForProcess ph))
    done <- newEmptyMVar
    -- The reader closes its own end: closing it here could block on the
    -- handle lock while the reader waits for an EOF that never comes.
    _ <- forkFinally (readCapped captureLimit readEnd) (\r -> hClose readEnd >> putMVar done r)
    -- One deadline covers both: a command can close its output and keep
    -- running, so end of output does not mean the process has exited.
    flip onException kill $
      timeout (secs * 1000000) ((,) <$> (takeMVar done >>= either throwIO pure) <*> waitForProcess ph) >>= \case
        Nothing -> kill >> pure Nothing
        Just (out, code) -> pure (Just (code, decodeUtf8Lenient out))

-- | Bytes of @bash@ output kept; the rest is read and discarded.
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

-- | Write via a temporary file and rename, so a process crash never leaves
-- a truncated file. There is no fsync, so power loss still can. Resolves symlinks first and keeps the existing mode.
-- The temporary file is created exclusively under a unique name, so a
-- symlink or a concurrent writer cannot redirect it.
atomicWrite :: FilePath -> BS.ByteString -> IO ()
atomicWrite path bytes = do
  target <- canonicalizePath path
  let dir = takeDirectory target
  createDirectoryIfMissing True dir
  existed <- doesFileExist target
  mode <- if existed then Just . fileMode <$> getFileStatus target else pure Nothing
  (tmp, h) <- openBinaryTempFileWithDefaultPermissions dir (takeFileName target <> ".hilda-tmp")
  flip onException (hClose h >> removeFile tmp) $ do
    -- The full mode, set before the content lands: directory's
    -- setPermissions copies only the owner bits, so a 0600 file became 0644.
    mapM_ (setFileMode tmp) mode
    BS.hPut h bytes
    hClose h
    renameFile tmp target

-- | Characters of tool output sent back to the model per call.
resultLimit :: Int
resultLimit = 30000

-- | Largest file @edit@ loads, in bytes.
editLimit :: Int
editLimit = 10 * 1024 * 1024

-- | Room for @read@ output, below 'resultLimit' so the agent never cuts it.
readBudget :: Int
readBudget = resultLimit - 200

-- | Number lines from @offset@ (1-based). Stops after @limit@ lines or
-- @budget@ characters and names the offset to continue from.
numberLines :: Int -> Int -> Int -> [Text] -> Text
numberLines budget offset limit ls =
  case drop (start - 1) (zip [1 :: Int ..] ls) of
    [] -> "(no lines at offset " <> tshow start <> "; the file is shorter)"
    rest -> T.unlines (go 0 budget rest)
  where
    start = max 1 offset
    go _ _ [] = []
    go n room ((i, l) : more)
      | n >= limit = [continueAt i]
      | cost <= room = numbered : go (n + 1) (room - cost) more
      | n > 0 = [continueAt i]
      | otherwise =
          -- A single line longer than the budget: return its start.
          T.take room numbered
            : ("[line " <> tshow i <> " cut to " <> tshow room <> " characters]")
            : [continueAt (i + 1) | not (null more)]
      where
        numbered = tshow i <> "\t" <> l
        cost = T.length numbered + 1
    continueAt i = "[more lines follow; continue with offset=" <> tshow i <> "]"

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
