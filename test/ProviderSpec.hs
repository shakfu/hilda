module ProviderSpec (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (isLeft)
import qualified Data.Text as T
import Data.IORef (modifyIORef, newIORef, readIORef)
import FakeServer
import Hilda.Provider
import Hilda.Types
import qualified Network.HTTP.Client as H
import Test.Hspec

-- | Decode an ASCII JSON fixture and run 'decodeReply' on it.
replyFrom :: String -> Either T.Text Reply
replyFrom s = either (error . ("bad fixture: " <>)) decodeReply (eitherDecode (BL.pack s))

keys :: Value -> [Key]
keys (Object o) = KM.keys o
keys _          = []

spec :: Spec
spec = do
  describe "encodeRequest" $ do
    it "asks for a stream and omits tools when there are none" $
      keys (encodeRequest OpenAICompatible (Request "m" [User "hi"] [])) `shouldMatchList` ["model", "messages", "stream", "stream_options"]
    it "sends tools with tool_choice auto" $ do
      let v = encodeRequest OpenAICompatible (Request "m" [] [object []])
      keys v `shouldMatchList` ["model", "messages", "stream", "stream_options", "tools", "tool_choice"]

  describe "cache_control" $ do
    let has kind model = "cache_control" `elem` keys (encodeRequest kind (Request model [] []))
    it "marks Anthropic models on OpenRouter" $
      has OpenRouter "anthropic/claude-x" `shouldBe` True
    it "leaves other models and providers alone" $ do
      has OpenRouter "openai/gpt-x" `shouldBe` False
      has OpenAICompatible "anthropic/claude-x" `shouldBe` False

  describe "message encoding" $ do
    it "encodes assistant tool calls in wire format" $
      toJSON (Assistant Nothing [ToolCall "c1" "read" "{\"path\":\"x\"}"])
        `shouldBe` object
          [ "role" .= ("assistant" :: String)
          , "content" .= Null
          , "tool_calls"
              .= [ object
                     [ "id" .= ("c1" :: String)
                     , "type" .= ("function" :: String)
                     , "function" .= object ["name" .= ("read" :: String), "arguments" .= ("{\"path\":\"x\"}" :: String)]
                     ]
                 ]
          ]
    it "encodes tool results with their call id" $
      toJSON (ToolResult "c1" "out")
        `shouldBe` object ["role" .= ("tool" :: String), "tool_call_id" .= ("c1" :: String), "content" .= ("out" :: String)]

  describe "decodeReply" $ do
    it "reads text and usage" $
      replyFrom "{\"choices\":[{\"message\":{\"content\":\"hi\"}}],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":4}}"
        `shouldBe` Right (Reply (Just "hi") [] (Usage 3 4 0 Nothing))
    it "reads OpenRouter's cost" $
      fmap replyUsage (replyFrom "{\"choices\":[{\"message\":{\"content\":\"hi\"}}],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":4,\"cost\":0.0021}}")
        `shouldBe` Right (Usage 3 4 0 (Just 0.0021))
    it "reads cached prompt tokens" $
      fmap replyUsage (replyFrom "{\"choices\":[{\"message\":{\"content\":\"hi\"}}],\"usage\":{\"prompt_tokens\":30,\"completion_tokens\":4,\"prompt_tokens_details\":{\"cached_tokens\":20}}}")
        `shouldBe` Right (Usage 30 4 20 Nothing)
    it "adds costs where reported" $ do
      Usage 1 2 0 (Just 0.5) <> Usage 3 4 0 Nothing `shouldBe` Usage 4 6 0 (Just 0.5)
      Usage 1 2 0 Nothing <> Usage 3 4 0 Nothing `shouldBe` Usage 4 6 0 Nothing
    it "treats blank content as absent" $
      replyFrom "{\"choices\":[{\"message\":{\"content\":\"  \"}}]}" `shouldBe` Right (Reply Nothing [] mempty)
    it "reads tool calls with string arguments" $
      replyFrom "{\"choices\":[{\"message\":{\"content\":null,\"tool_calls\":[{\"id\":\"a\",\"type\":\"function\",\"function\":{\"name\":\"bash\",\"arguments\":\"{\\\"command\\\":\\\"ls\\\"}\"}}]}}]}"
        `shouldBe` Right (Reply Nothing [ToolCall "a" "bash" "{\"command\":\"ls\"}"] mempty)
    it "accepts tool arguments sent as an object" $
      replyFrom "{\"choices\":[{\"message\":{\"tool_calls\":[{\"id\":\"a\",\"function\":{\"name\":\"bash\",\"arguments\":{\"command\":\"ls\"}}}]}}]}"
        `shouldBe` Right (Reply Nothing [ToolCall "a" "bash" "{\"command\":\"ls\"}"] mempty)
    it "reports an error object returned with status 200" $
      replyFrom "{\"error\":{\"message\":\"rate limited\",\"code\":429}}"
        `shouldSatisfy` either (T.isInfixOf "rate limited") (const False)
    it "rejects a response without choices" $
      replyFrom "{\"choices\":[]}" `shouldSatisfy` isLeft

  describe "retries" $ do
    it "retries only 429 among statuses" $
      map retryableStatus [429, 500, 502, 503, 400] `shouldBe` [True, False, False, False, False]
    it "retries connection failures but not response timeouts" $ do
      retryableError (H.HttpExceptionRequest H.defaultRequest H.ConnectionTimeout) `shouldBe` True
      retryableError (H.HttpExceptionRequest H.defaultRequest H.ResponseTimeout) `shouldBe` False

  describe "send" $ do
    let ok = json 200 "{\"choices\":[{\"message\":{\"content\":\"ok\"}}]}"
        call port = do
          Right complete <- newComplete (Provider OpenAICompatible ("http://127.0.0.1:" <> show port <> "/v1") Nothing)
          seen <- newIORef []
          r <- complete (\d -> modifyIORef seen (d :)) (Request "m" [User "hi"] [])
          (,) r . reverse <$> readIORef seen
        serve responses = do
          port <- freePort
          withServer port 0 responses (call port)
    it "retries a 429 and then succeeds" $ do
      ((r, _), n) <- serve [json 429 "{}", ok]
      fmap replyText r `shouldBe` Right (Just "ok")
      n `shouldBe` 2
    it "does not retry a 500" $ do
      ((r, _), n) <- serve [json 500 "boom", ok]
      r `shouldBe` Left "HTTP 500: boom"
      n `shouldBe` 1
    it "does not retry a 400" $ do
      ((r, _), n) <- serve [json 400 "bad", ok]
      r `shouldSatisfy` either (T.isPrefixOf "HTTP 400") (const False)
      n `shouldBe` 1
    it "retries a refused connection until the server is up" $ do
      port <- freePort
      ((r, _), n) <- withServer port 300000 [ok] (call port)
      fmap replyText r `shouldBe` Right (Just "ok")
      n `shouldBe` 1
    it "streams server-sent events to the sink" $ do
      let events =
            "data: {\"choices\":[{\"delta\":{\"content\":\"Hel\"}}]}\n\n\
            \: keep-alive\n\n\
            \data: {\"choices\":[{\"delta\":{\"content\":\"lo\"}}]}\n\n\
            \data: {\"choices\":[],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":1}}\n\n\
            \data: [DONE]\n\n"
      ((r, deltas), _) <- serve [Canned 200 "text/event-stream" events]
      r `shouldBe` Right (Reply (Just "Hello") [] (Usage 3 1 0 Nothing))
      deltas `shouldBe` [TextDelta "Hel", TextDelta "lo"]
    it "passes a plain JSON reply to the sink in one piece" $ do
      ((_, deltas), _) <- serve [ok]
      deltas `shouldBe` [TextDelta "ok"]

  describe "parseKind" $
    it "round-trips every kind" $
      map (parseKind . kindName) [minBound .. maxBound] `shouldBe` map Just [minBound .. maxBound]
