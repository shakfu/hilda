-- | Settings remembered between runs: the last model used per provider.
module Hilda.State
  ( stateFile
  , loadModels
  , rememberModel
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Hilda.Provider (ProviderKind, kindName)
import Hilda.Tools (atomicWrite)
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.FilePath ((</>))

stateFile :: IO FilePath
stateFile = (</> "models.json") <$> getXdgDirectory XdgState "hilda"

-- | Provider name to model. A missing or unreadable file reads as empty.
loadModels :: FilePath -> IO (Map Text Text)
loadModels path =
  try @IOException (BS.readFile path) >>= \case
    Left _ -> pure Map.empty
    Right bytes -> pure (fromMaybe Map.empty (decodeStrict bytes))

rememberModel :: FilePath -> ProviderKind -> Text -> IO ()
rememberModel path kind model = do
  models <- loadModels path
  atomicWrite path (BL.toStrict (encode (Map.insert (kindName kind) model models)))
