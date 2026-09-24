module StateSpec (spec) where

import Data.Bits ((.&.))
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Hilda.Provider (ProviderKind (..))
import Hilda.State
import System.Directory (listDirectory)
import Hilda.Types
import System.FilePath (takeFileName, (</>))
import System.Posix.Files (fileMode, getFileStatus)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  it "reads a missing file as empty" $ withSystemTempDirectory "hilda" $ \dir ->
    loadMap (dir </> "none.json") `shouldReturn` (Map.empty :: Map.Map Text Text)

  it "reads a corrupt file as empty" $ withSystemTempDirectory "hilda" $ \dir -> do
    let path = dir </> "models.json"
    writeFile path "{not json"
    loadMap path `shouldReturn` (Map.empty :: Map.Map Text Text)

  it "remembers the last model per provider" $ withSystemTempDirectory "hilda" $ \dir -> do
    let path = dir </> "sub" </> "models.json"
    rememberModel path OpenRouter "a/one"
    rememberModel path OpenAICompatible "local"
    rememberModel path OpenRouter "b/two"
    loadMap path `shouldReturn` Map.fromList [("openrouter", "b/two" :: Text), ("openai", "local")]
    listDirectory (dir </> "sub") `shouldReturn` ["models.json"]

  it "remembers context windows as numbers" $ withSystemTempDirectory "hilda" $ \dir -> do
    let path = dir </> "contexts.json"
    remember path "a/one" (8192 :: Int)
    remember path "b/two" (200000 :: Int)
    loadMap path `shouldReturn` Map.fromList [("a/one", 8192 :: Int), ("b/two", 200000)]

  describe "sessions" $ do
    let msgs = [User "go", Assistant Nothing [ToolCall "c1" "read" "{}"] [], ToolResult "c1" "out", Assistant (Just "done") [] []]
    it "round-trips a history in a private directory" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "sessions" </> "s.json"
      saveSession path msgs
      loadSession path `shouldReturn` Right msgs
      ((.&. 0o777) . fileMode <$> getFileStatus (dir </> "sessions")) `shouldReturn` 0o700
    it "reports a missing or corrupt session" $ withSystemTempDirectory "hilda" $ \dir -> do
      loadSession (dir </> "none.json") >>= (`shouldSatisfy` isLeft)
      writeFile (dir </> "bad.json") "[{"
      loadSession (dir </> "bad.json") >>= (`shouldSatisfy` isLeft)
    it "names the file after the escaped working directory" $
      takeFileName <$> sessionFile "/a/b%c" `shouldReturn` "%2Fa%2Fb%25c.json"
