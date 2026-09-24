module ReplSpec (spec) where

import Data.IORef (newIORef, readIORef)
import Hilda.Repl
import Hilda.Types
import Test.Hspec

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
