-- | Settings remembered between runs: the last model used per provider,
-- context windows looked up for models, and the REPL history of each
-- working directory or session id.
module Hilda.State
  ( stateFile
  , contextsFile
  , loadMap
  , remember
  , rememberModel
  , sessionFile
  , saveSession
  , loadSession
  , newSessionId
  , idSessionFile
  , saveIdSession
  , loadIdSession
  , sessionDays
  , pruneIdSessions
  ) where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Data.Aeson (FromJSON, ToJSON, Value, decodeStrict, eitherDecodeStrict, encode, object, toJSON, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hilda.Provider (ProviderKind, kindName)
import Hilda.Tools (atomicWrite)
import Hilda.Types (Message)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDay)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getModificationTime, getXdgDirectory, listDirectory, removeFile)
import System.FilePath (splitExtension, takeDirectory, (</>))
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.Posix.Files (setFileMode)
import Text.Printf (printf)

-- | @$XDG_STATE_HOME/hilda/models.json@.
stateFile :: IO FilePath
stateFile = (</> "models.json") <$> getXdgDirectory XdgState "hilda"

-- | @$XDG_STATE_HOME/hilda/contexts.json@: model to context window, in tokens.
contextsFile :: IO FilePath
contextsFile = (</> "contexts.json") <$> getXdgDirectory XdgState "hilda"

-- | A JSON object as a map. A missing or unreadable file reads as empty.
loadMap :: FromJSON v => FilePath -> IO (Map Text v)
loadMap path =
  try @IOException (BS.readFile path) >>= \case
    Left _ -> pure Map.empty
    Right bytes -> pure (fromMaybe Map.empty (decodeStrict bytes))

-- | Set one key in a map file, keeping the other entries.
remember :: ToJSON v => FilePath -> Text -> v -> IO ()
remember path k v = do
  m <- loadMap path
  atomicWrite path (BL.toStrict (encode (Map.insert k (toJSON v) (m :: Map Text Value))))

-- | Record the model for a provider, keeping other providers' entries.
rememberModel :: FilePath -> ProviderKind -> Text -> IO ()
rememberModel path kind = remember path (kindName kind)

-- | Where the REPL history of @cwd@ is saved: one file per directory under
-- @$XDG_STATE_HOME/hilda/sessions/@, the path percent-escaped.
sessionFile :: FilePath -> IO FilePath
sessionFile cwd = (</> ("sessions" </> concatMap escape cwd <> ".json")) <$> getXdgDirectory XdgState "hilda"
  where
    escape = \case
      '/' -> "%2F"
      '%' -> "%25"
      c -> [c]

-- | Save a history. The REPL leaves out the system prompt, which is
-- rebuilt from the current options on resume.
saveSession :: FilePath -> [Message] -> IO ()
saveSession = writePrivate

-- | Write JSON to a file in a 0700 directory.
writePrivate :: ToJSON a => FilePath -> a -> IO ()
writePrivate path v = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  -- Transcripts hold file contents and command output.
  setFileMode dir 0o700
  atomicWrite path (BL.toStrict (encode v))

-- | A saved history, or why there is none.
loadSession :: FilePath -> IO (Either Text [Message])
loadSession path =
  try @IOException (BS.readFile path) >>= \case
    Left _ -> pure (Left "no saved session for this directory")
    Right bytes -> pure (first (const "the saved session is unreadable") (eitherDecodeStrict bytes))

-- | A new session id: 16 hex digits from @/dev/urandom@.
newSessionId :: IO Text
newSessionId = T.pack . concatMap (printf "%02x") . BS.unpack <$> withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 8)

-- | Where session @sid@ is saved, beside the per-directory files, whose
-- names start with @%2F@. Ids are lowercase hex, so none names a path.
idSessionFile :: Text -> IO (Either Text FilePath)
idSessionFile sid
  | not (isSessionId (T.unpack sid)) = pure (Left ("invalid session id '" <> sid <> "'"))
  | otherwise = Right . (</> ("sessions" </> T.unpack sid <> ".json")) <$> getXdgDirectory XdgState "hilda"

-- | Is this a lowercase hex session id?
isSessionId :: String -> Bool
isSessionId sid = not (null sid) && all (`elem` ("0123456789abcdef" :: String)) sid

-- | How many days an id session is kept, from @HILDA_SESSION_DAYS@
-- (default 30; 0 keeps every session), or why the value is refused.
sessionDays :: Maybe String -> Either Text Int
sessionDays Nothing = Right 30
sessionDays (Just v) = case reads v of
  [(n, "")] | n >= 0 -> Right n
  _ -> Left ("HILDA_SESSION_DAYS must be a whole number of days, 0 to keep every session; got '" <> T.pack v <> "'")

-- | Delete the id sessions last saved more than @days@ days ago, and
-- answer how many went. 0 deletes none. The per-directory REPL files stay:
-- there is one per directory, and @-c@ reads them.
pruneIdSessions :: Int -> IO Int
pruneIdSessions days
  | days <= 0 = pure 0
  | otherwise = do
      dir <- (</> "sessions") <$> getXdgDirectory XdgState "hilda"
      now <- getCurrentTime
      names <- either (const []) id <$> try @IOException (listDirectory dir)
      let stale name = case splitExtension name of
            (sid, ".json") | isSessionId sid -> do
              saved <- try @IOException (getModificationTime (dir </> name))
              pure (either (const False) (\t -> diffUTCTime now t > fromIntegral days * nominalDay) saved)
            _ -> pure False
          remove name = either (const 0) (const 1) <$> try @IOException (removeFile (dir </> name))
      filterM stale names >>= fmap sum . mapM remove

-- | Save a history with the working directory it ran in and, if given,
-- the session's cost so far.
saveIdSession :: FilePath -> FilePath -> Maybe Double -> [Message] -> IO ()
saveIdSession path cwd cost msgs = writePrivate path (object (["cwd" .= cwd, "messages" .= msgs] <> maybe [] (\c -> ["cost" .= c]) cost))

-- | A history saved by 'saveIdSession' in @cwd@ and its cost, or why
-- there is none. Another directory's session is refused: its history
-- names files there.
loadIdSession :: FilePath -> FilePath -> IO (Either Text ([Message], Maybe Double))
loadIdSession path cwd =
  try @IOException (BS.readFile path) >>= \case
    Left _ -> pure (Left "no saved session with this id")
    Right bytes -> pure $ case eitherDecodeStrict bytes >>= parseEither saved of
      Left _ -> Left "the saved session is unreadable"
      Right (dir, msgs, cost)
        | dir /= cwd -> Left ("the session was saved in " <> T.pack dir <> "; resume it there")
        | otherwise -> Right (msgs, cost)
  where
    saved = withObject "session" $ \o -> (,,) <$> o .: "cwd" <*> o .: "messages" <*> o .:? "cost"
