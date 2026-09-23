module ReplSpec (spec) where

import Hilda.Repl
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
