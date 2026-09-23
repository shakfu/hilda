module RenderSpec (spec) where

import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import Hilda.Agent (Event (..))
import Hilda.Render
import Hilda.Types
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Test.Hspec

spec :: Spec
spec = do
  describe "renderEvent" $ do
    let bash = ToolCall "c1" "bash" "{\"command\":\"git status\"}"
    it "starts a tool line with the tool and its main argument" $
      renderEvent plain (CallStarted bash) `shouldBe` Partial "[bash] git status "
    it "finishes the line with an estimated token count" $
      renderEvent plain (CallFinished bash (Right (T.replicate 40 "x"))) `shouldBe` Full "-> ~10"
    it "reports context trimming" $ do
      renderEvent plain (ContextTrimmed 2 4000) `shouldBe` Full "[context: elided 2 old tool messages, ~1000 tokens]"
      renderEvent plain (ContextTrimmed 1 400) `shouldBe` Full "[context: elided 1 old tool message, ~100 tokens]"
    it "warns when a cost limit cannot work" $
      renderEvent plain CostUnknown `shouldBe` Full "[--max-cost has no effect: the provider reports no cost]"
    it "finishes the line with the error" $
      renderEvent plain (CallFinished bash (Left "boom")) `shouldBe` Full "-> error: boom"
    it "wraps colored text in ANSI codes" $ do
      ansi Red "x" `shouldBe` "\ESC[31mx\ESC[0m"
      ansi BoldMagenta "x" `shouldBe` "\ESC[1;35mx\ESC[0m"

  describe "callSummary" $ do
    it "uses the path for file tools" $
      callSummary (ToolCall "c" "read" "{\"path\":\"src/A.hs\",\"limit\":5}") `shouldBe` "src/A.hs"
    it "escapes control characters in the tool line" $
      callSummary (ToolCall "c" "bash" "{\"command\":\"a\\u001b[2Kb\"}") `shouldBe` "a\\x1b[2Kb"
    it "falls back to the raw arguments" $
      callSummary (ToolCall "c" "x" "{\"n\":1}") `shouldBe` "{\"n\":1}"
    it "flattens and elides long commands to 80 characters" $ do
      let s = callSummary (ToolCall "c" "bash" ("{\"command\":\"" <> T.replicate 100 "a" <> "\\nb\"}"))
      T.length s `shouldBe` 80
      s `shouldSatisfy` T.isSuffixOf ".."

  describe "confirmDetail" $ do
    it "shows the whole command, however long" $ do
      let cmd = "echo ok" <> T.replicate 100 " " <> "rm -rf ~"
      confirmDetail (ToolCall "c" "bash" ("{\"command\":\"" <> cmd <> "\"}"))
        `shouldBe` "  command: " <> cmd
    it "keeps newlines and escapes other control characters" $
      confirmDetail (ToolCall "c" "bash" "{\"command\":\"ls\\rrm x\\necho\"}")
        `shouldBe` "  command: ls\\x0drm x\n    echo"
    it "lists the path first and summarises file content" $
      confirmDetail (ToolCall "c" "write" "{\"content\":\"abc\",\"path\":\"a.txt\"}")
        `shouldBe` "  path: a.txt\n  content: <3 characters>"
    it "shows unparseable arguments verbatim" $
      confirmDetail (ToolCall "c" "bash" "{oops") `shouldBe` "  arguments: {oops"

  describe "liveOutput" $ do
    let reply = Right (Reply Nothing [] mempty)
        req = Request "m" [] []
        streams delay = \sink _ -> threadDelay delay >> sink (TextDelta "hel") >> sink (TextDelta "lo") >> pure reply
        run live backend = withSystemTempFile "live" $ \path h -> do
          seen <- newIORef []
          _ <- liveOutput h plain live backend (\d -> modifyIORef seen (d :)) req
          hClose h
          (,) <$> readFile path <*> (reverse <$> readIORef seen)
    it "erases the waiting line before streamed text and ends the line" $
      run (Live True True) (streams 0) `shouldReturn` ("\r\ESC[Khello\n", [TextDelta "hel", TextDelta "lo"])
    it "counts seconds until the first text" $
      fmap fst (run (Live True True) (streams 1500000)) `shouldReturn` "\r[waiting 1s]\r\ESC[Khello\n"
    it "only erases when not echoing" $
      run (Live True False) (streams 0) `shouldReturn` ("\r\ESC[K", [TextDelta "hel", TextDelta "lo"])
    it "says thinking once reasoning arrives" $ do
      let thinks = \sink _ -> sink (ReasoningDelta "hmm") >> threadDelay 1500000 >> sink (TextDelta "ok") >> pure reply
      fmap fst (run (Live True True) thinks) `shouldReturn` "\r[thinking 1s]\r\ESC[Kok\n"
    it "echoes without escape codes when there is no ticker" $
      fmap fst (run (Live False True) (streams 0)) `shouldReturn` "hello\n"

  describe "renderUsage" $ do
    it "omits cost when the provider reports none" $
      renderUsage (Usage 1200 30 0 Nothing) `shouldBe` "1200 in / 30 out"
    it "shows cost when reported" $
      renderUsage (Usage 1200 30 0 (Just 0.0213)) `shouldBe` "1200 in / 30 out, $0.0213"
    it "shows cached tokens when there are any" $
      renderUsage (Usage 1200 30 900 Nothing) `shouldBe` "1200 in (900 cached) / 30 out"
    it "keeps precision below one cent" $
      formatCost 0.000123 `shouldBe` "$0.000123"
