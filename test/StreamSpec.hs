module StreamSpec (spec) where

import Data.Aeson (Value, eitherDecodeStrict)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (foldlM)
import Data.Text (Text)
import Hilda.Stream
import Hilda.Types
import Test.Hspec

-- | Fold JSON chunks into a reply, collecting the text deltas.
fold :: [String] -> Either Text (Reply, [Delta])
fold chunks = do
  values <- traverse decode chunks
  (p, deltas) <- foldlM step (emptyPartial, []) values
  pure (finishPartial p, reverse deltas)
  where
    decode :: String -> Either Text Value
    decode = first (const "bad fixture") . eitherDecodeStrict . BS8.pack
    step (p, ds) v = (\(p', d) -> (p', reverse d <> ds)) <$> stepChunk p v

spec :: Spec
spec = do
  describe "sseData" $ do
    it "returns complete data lines and keeps the remainder" $
      sseData "data: {\"a\":1}\r\n\n: keep-alive\nevent: x\ndata:[DONE]\ndata: {\"b\""
        `shouldBe` (["{\"a\":1}", "[DONE]"], "data: {\"b\"")
    it "returns nothing for a buffer without a newline" $
      sseData "data: {" `shouldBe` ([], "data: {")

  describe "stepChunk" $ do
    it "joins text deltas and takes usage from the last chunk" $
      fold
        [ "{\"choices\":[{\"delta\":{\"content\":\"Hel\"}}],\"usage\":null}"
        , "{\"choices\":[{\"delta\":{\"content\":\"lo\"}}]}"
        , "{\"choices\":[],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2,\"cost\":0.5}}"
        ]
        `shouldBe` Right (Reply (Just "Hello") [] (Usage 3 2 0 (Just 0.5)), [TextDelta "Hel", TextDelta "lo"])

    it "reads reasoning from reasoning_details" $
      fmap snd (fold ["{\"choices\":[{\"delta\":{\"reasoning_details\":[{\"type\":\"reasoning.text\",\"text\":\"hmm\"}]}}]}"])
        `shouldBe` Right [ReasoningDelta "hmm"]
    it "reads a plain reasoning string" $
      fmap snd (fold ["{\"choices\":[{\"delta\":{\"reasoning\":\"hmm\"}}]}"])
        `shouldBe` Right [ReasoningDelta "hmm"]
    it "does not count reasoning sent in both forms twice" $
      fmap snd (fold ["{\"choices\":[{\"delta\":{\"reasoning\":\"hmm\",\"reasoning_details\":[{\"type\":\"reasoning.text\",\"text\":\"hmm\"}]}}]}"])
        `shouldBe` Right [ReasoningDelta "hmm"]
    it "keeps reasoning out of the reply text" $
      fmap (replyText . fst) (fold ["{\"choices\":[{\"delta\":{\"reasoning\":\"hmm\",\"content\":\"ok\"}}]}"])
        `shouldBe` Right (Just "ok")

    it "merges tool call pieces by index" $
      fmap (replyCalls . fst) (fold
        [ "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"a\",\"function\":{\"name\":\"bash\",\"arguments\":\"\"}}]}}]}"
        , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":1,\"id\":\"b\",\"function\":{\"name\":\"read\",\"arguments\":\"{}\"}}]}}]}"
        , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"{\\\"command\\\":\"}}]}}]}"
        , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"ls\\\"}\"}}]}}]}"
        ])
        `shouldBe` Right [ToolCall "a" "bash" "{\"command\":\"ls\"}", ToolCall "b" "read" "{}"]

    it "groups pieces without an index by id" $
      fmap (replyCalls . fst) (fold
        [ "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"id\":\"a\",\"function\":{\"name\":\"bash\",\"arguments\":\"{\\\"command\\\"\"}}]}}]}"
        , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"function\":{\"arguments\":\":\\\"ls\\\"}\"}}]}}]}"
        , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"id\":\"b\",\"function\":{\"name\":\"read\"}}]}}]}"
        ])
        `shouldBe` Right [ToolCall "a" "bash" "{\"command\":\"ls\"}", ToolCall "b" "read" "{}"]

    it "names calls that arrive without an id" $
      fmap (replyCalls . fst) (fold ["{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"name\":\"read\"}}]}}]}"])
        `shouldBe` Right [ToolCall "call_0" "read" "{}"]

    it "fails on an error chunk" $
      fold ["{\"error\":{\"message\":\"overloaded\"}}"] `shouldBe` Left "Error in $: provider error: overloaded"
