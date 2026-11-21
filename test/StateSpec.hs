module StateSpec (spec) where

import Data.Bits ((.&.))
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
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
    it "makes distinct 16-digit hex ids that name a session file" $ do
      a <- newSessionId
      b <- newSessionId
      T.length a `shouldBe` 16
      a `shouldNotBe` b
      fmap takeFileName <$> idSessionFile a `shouldReturn` Right (T.unpack a <> ".json")
    it "rejects ids that are not lowercase hex" $
      mapM_ (\sid -> idSessionFile sid >>= (`shouldSatisfy` isLeft)) ["", "../x", "AB12", "a/b"]
    it "round-trips a session saved by id in its directory only" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "sessions" </> "ab12.json"
      saveIdSession path "/work" Nothing msgs
      loadIdSession path "/work" `shouldReturn` Right (msgs, Nothing)
      loadIdSession path "/elsewhere" >>= (`shouldSatisfy` isLeft)
      loadIdSession (dir </> "none.json") "/work" >>= (`shouldSatisfy` isLeft)
      writeFile (dir </> "bad.json") "[]"
      loadIdSession (dir </> "bad.json") "/work" >>= (`shouldSatisfy` isLeft)
    it "keeps a session's cost; a file without one reads as none" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "sessions" </> "ab12.json"
      saveIdSession path "/work" (Just 0.003) msgs
      loadIdSession path "/work" `shouldReturn` Right (msgs, Just 0.003)
      writeFile path "{\"cwd\":\"/work\",\"messages\":[]}"
      loadIdSession path "/work" `shouldReturn` Right ([], Nothing)
    it "names the file after the escaped working directory" $
      takeFileName <$> sessionFile "/a/b%c" `shouldReturn` "%2Fa%2Fb%25c.json"
    it "keeps id sessions 30 days unless HILDA_SESSION_DAYS says otherwise" $ do
      sessionDays Nothing `shouldBe` Right 30
      sessionDays (Just "7") `shouldBe` Right 7
      sessionDays (Just "0") `shouldBe` Right 0
      mapM_ ((`shouldSatisfy` isLeft) . sessionDays . Just) ["", "-1", "7d", "1.5"]
