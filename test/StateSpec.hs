module StateSpec (spec) where

import qualified Data.Map.Strict as Map
import Hilda.Provider (ProviderKind (..))
import Hilda.State
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  it "reads a missing file as empty" $ withSystemTempDirectory "hilda" $ \dir ->
    loadModels (dir </> "none.json") `shouldReturn` Map.empty

  it "reads a corrupt file as empty" $ withSystemTempDirectory "hilda" $ \dir -> do
    let path = dir </> "models.json"
    writeFile path "{not json"
    loadModels path `shouldReturn` Map.empty

  it "remembers the last model per provider" $ withSystemTempDirectory "hilda" $ \dir -> do
    let path = dir </> "sub" </> "models.json"
    rememberModel path OpenRouter "a/one"
    rememberModel path OpenAICompatible "local"
    rememberModel path OpenRouter "b/two"
    loadModels path `shouldReturn` Map.fromList [("openrouter", "b/two"), ("openai", "local")]
    listDirectory (dir </> "sub") `shouldReturn` ["models.json"]
