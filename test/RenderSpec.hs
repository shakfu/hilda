module RenderSpec (spec) where

import qualified Data.Text as T
import Hilda.Agent (Event (..))
import Hilda.Render
import Hilda.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "renderEvent" $ do
    let bash = ToolCall "c1" "bash" "{\"command\":\"git status\"}"
    it "starts a tool line with the tool and its main argument" $
      renderEvent plain (CallStarted bash) `shouldBe` Partial "[bash] git status "
    it "finishes the line with an estimated token count" $
      renderEvent plain (CallFinished bash (Right (T.replicate 40 "x"))) `shouldBe` Full "-> ~10 tokens"
    it "finishes the line with the error" $
      renderEvent plain (CallFinished bash (Left "boom")) `shouldBe` Full "-> error: boom"
    it "wraps colored text in ANSI codes" $ do
      ansi Red "x" `shouldBe` "\ESC[31mx\ESC[0m"
      ansi BoldMagenta "x" `shouldBe` "\ESC[1;35mx\ESC[0m"

  describe "callSummary" $ do
    it "uses the path for file tools" $
      callSummary (ToolCall "c" "read" "{\"path\":\"src/A.hs\",\"limit\":5}") `shouldBe` "src/A.hs"
    it "falls back to the raw arguments" $
      callSummary (ToolCall "c" "x" "{\"n\":1}") `shouldBe` "{\"n\":1}"
    it "flattens and elides long commands to 80 characters" $ do
      let s = callSummary (ToolCall "c" "bash" ("{\"command\":\"" <> T.replicate 100 "a" <> "\\nb\"}"))
      T.length s `shouldBe` 80
      s `shouldSatisfy` T.isSuffixOf ".."

  describe "renderUsage" $ do
    it "omits cost when the provider reports none" $
      renderUsage (Usage 1200 30 Nothing) `shouldBe` "1200 in / 30 out"
    it "shows cost when reported" $
      renderUsage (Usage 1200 30 (Just 0.0213)) `shouldBe` "1200 in / 30 out, $0.0213"
    it "keeps precision below one cent" $
      formatCost 0.000123 `shouldBe` "$0.000123"
