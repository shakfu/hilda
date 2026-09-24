-- | Settings remembered between runs: the last model used per provider,
-- context windows looked up for models, and the REPL history of each
-- working directory.
module Hilda.State
  ( stateFile
  , contextsFile
  , loadMap
  , remember
  , rememberModel
  , sessionFile
  , saveSession
  , loadSession
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, Value, decodeStrict, eitherDecodeStrict, encode, toJSON)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Hilda.Provider (ProviderKind, kindName)
import Hilda.Tools (atomicWrite)
import Hilda.Types (Message)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)

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
saveSession path msgs = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  -- Transcripts hold file contents and command output.
  setFileMode dir 0o700
  atomicWrite path (BL.toStrict (encode msgs))

-- | A saved history, or why there is none.
loadSession :: FilePath -> IO (Either Text [Message])
loadSession path =
  try @IOException (BS.readFile path) >>= \case
    Left _ -> pure (Left "no saved session for this directory")
    Right bytes -> pure (first (const "the saved session is unreadable") (eitherDecodeStrict bytes))
