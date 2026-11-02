module ReplSpec (spec) where

import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Hilda.App (Config (..))
import Hilda.Policy (Mode (..))
import Hilda.Provider (ProviderKind (..))
import Hilda.Render (When (..), plain)
import Hilda.Repl
import Hilda.Types
import Test.Hspec

-- | Run 'interactive' with @initial@, then the typed @rest@, against a
-- backend that answers "ok". Returns the user messages the backend saw
-- last and the history passed to save last.
session :: Maybe Text -> [Text] -> IO ([Message], [Message])
session initial rest = do
  lines' <- newIORef rest
  seen <- newIORef []
  saved <- newIORef []
  spent <- newIORef mempty
  let readLine = atomicModifyIORef' lines' (\case [] -> ([], Nothing); (l : ls) -> (ls, Just l))
      backend _ r = do
        modifyIORef' seen (const [m | m@(User _) <- reqMessages r])
        pure (Right (Reply (Just "ok") [] (Usage 1 1 0 Nothing) []))
      cfg = Config backend OpenAICompatible "m" Yolo "sys" 5 (const (pure 1000)) Nothing (const (pure ())) False Never
  interactive readLine initial plain spent [] (modifyIORef' saved . const) cfg
  (,) <$> readIORef seen <*> readIORef saved

spec :: Spec
spec = do
  it "treats plain text as a prompt" $
    parseInput "fix the build" `shouldBe` Prompt "fix the build"
  it "parses commands and their argument" $ do
    parseInput "/mode ask" `shouldBe` Run (SetMode (Just "ask"))
    parseInput "/mode" `shouldBe` Run (SetMode Nothing)
    parseInput "/model gpt-x" `shouldBe` Run (SetModel (Just "gpt-x"))
    parseInput "/exit" `shouldBe` Run Quit
  it "flags unknown commands" $
    parseInput "/frob" `shouldBe` Run (Unknown "frob")
  it "sends // as a prompt starting with /" $
    parseInput "//etc/hosts is wrong" `shouldBe` Prompt "/etc/hosts is wrong"
  it "tallies the usage of every completed call" $ do
    ref <- newIORef mempty
    let backend _ _ = pure (Right (Reply Nothing [] (Usage 10 2 0 (Just 0.5)) []))
        call = tally ref backend (const (pure ())) (Request "m" [] [])
    _ <- call
    _ <- call
    readIORef ref `shouldReturn` Usage 20 4 0 (Just 1.0)
  describe "interactive" $ do
    it "submits the initial entry as the first prompt" $ do
      (seen, saved) <- session (Just "fix the build") []
      seen `shouldBe` [User "fix the build"]
      saved `shouldBe` [User "fix the build", Assistant (Just "ok") [] []]
    it "continues with typed lines after the initial entry" $ do
      (seen, _) <- session (Just "first") ["second"]
      seen `shouldBe` [User "first", User "second"]
    it "ignores an all-whitespace initial entry, like an empty line" $ do
      (seen, _) <- session (Just "  \t ") ["typed"]
      seen `shouldBe` [User "typed"]
    it "parses the initial entry as a typed line" $ do
      (quit, _) <- session (Just "/quit") ["not read"]
      quit `shouldBe` []
      (escaped, _) <- session (Just "//etc/hosts") []
      escaped `shouldBe` [User "/etc/hosts"]
