module ReplSpec (spec) where

import Hilda.Repl
import Test.Hspec

spec :: Spec
spec = do
  it "treats plain text as a prompt" $
    parseCommand "fix the build" `shouldBe` Nothing
  it "parses commands and their argument" $ do
    parseCommand "/mode ask" `shouldBe` Just (SetMode (Just "ask"))
    parseCommand "/mode" `shouldBe` Just (SetMode Nothing)
    parseCommand "/model gpt-x" `shouldBe` Just (SetModel (Just "gpt-x"))
    parseCommand "/exit" `shouldBe` Just Quit
  it "flags unknown commands" $
    parseCommand "/frob" `shouldBe` Just (Unknown "frob")
