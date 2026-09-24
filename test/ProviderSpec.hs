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
import System.Timeout (timeout)
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
      keys (encodeRequest OpenAICompatible Nothing (Request "m" [User "hi"] [])) `shouldMatchList` ["model", "messages", "stream", "stream_options"]
    it "sends tools with tool_choice auto" $ do
      let v = encodeRequest OpenAICompatible Nothing (Request "m" [] [object []])
      keys v `shouldMatchList` ["model", "messages", "stream", "stream_options", "tools", "tool_choice"]

  describe "reasoning effort" $ do
    let field kind k = KM.lookup k =<< (\case Object o -> Just o; _ -> Nothing) (encodeRequest kind (Just "high") (Request "m" [] []))
    it "sends reasoning.effort to OpenRouter" $
      field OpenRouter "reasoning" `shouldBe` Just (object ["effort" .= ("high" :: String)])
    it "sends reasoning_effort to OpenAI-compatible servers" $
      field OpenAICompatible "reasoning_effort" `shouldBe` Just (String "high")
    it "sends neither without an effort" $
      keys (encodeRequest OpenRouter Nothing (Request "m" [] [])) `shouldNotContain` ["reasoning"]

  describe "cache_control" $ do
    let has kind model = "cache_control" `elem` keys (encodeRequest kind Nothing (Request model [] []))
    it "marks Anthropic models on OpenRouter" $
      has OpenRouter "anthropic/claude-x" `shouldBe` True
    it "leaves other models and providers alone" $ do
      has OpenRouter "openai/gpt-x" `shouldBe` False
      has OpenAICompatible "anthropic/claude-x" `shouldBe` False

  describe "message encoding" $ do
    it "reads messages back from the wire format" $ do
      let msgs =
            [ System "s", User "u"
            , Assistant (Just "t") [ToolCall "c1" "read" "{\"path\":\"x\"}"] [object ["type" .= ("reasoning.text" :: String)]]
            , ToolResult "c1" "out", Assistant (Just "done") [] [], Assistant Nothing [] []
            ]
      eitherDecode (encode msgs) `shouldBe` Right msgs
    it "encodes assistant tool calls in wire format" $
      toJSON (Assistant Nothing [ToolCall "c1" "read" "{\"path\":\"x\"}"] [])
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

  describe "reasoning_details" $ do
    let r = object ["type" .= ("reasoning.text" :: String), "text" .= ("t" :: String)]
    it "sends the blocks back on the assistant message" $
      toJSON (Assistant Nothing [] [r])
        `shouldBe` object ["role" .= ("assistant" :: String), "content" .= ("" :: String), "reasoning_details" .= [r]]
    it "reads the blocks of a plain JSON reply" $
      fmap replyReasoning (replyFrom "{\"choices\":[{\"message\":{\"content\":\"hi\",\"reasoning_details\":[{\"type\":\"reasoning.text\",\"text\":\"t\"}]}}]}")
        `shouldBe` Right [r]
    it "drops the blocks unless kept" $ do
      let backend _ _ = pure (Right (Reply Nothing [] mempty [r]))
      fmap replyReasoning <$> withoutReasoning backend (const (pure ())) (Request "m" [] [])
        `shouldReturn` Right []

  describe "decodeReply" $ do
    it "reads text and usage" $
      replyFrom "{\"choices\":[{\"message\":{\"content\":\"hi\"}}],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":4}}"
        `shouldBe` Right (Reply (Just "hi") [] (Usage 3 4 0 Nothing) [])
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
      replyFrom "{\"choices\":[{\"message\":{\"content\":\"  \"}}]}" `shouldBe` Right (Reply Nothing [] mempty [])
    it "reads tool calls with string arguments" $
      replyFrom "{\"choices\":[{\"message\":{\"content\":null,\"tool_calls\":[{\"id\":\"a\",\"type\":\"function\",\"function\":{\"name\":\"bash\",\"arguments\":\"{\\\"command\\\":\\\"ls\\\"}\"}}]}}]}"
        `shouldBe` Right (Reply Nothing [ToolCall "a" "bash" "{\"command\":\"ls\"}"] mempty [])
    it "accepts tool arguments sent as an object" $
      replyFrom "{\"choices\":[{\"message\":{\"tool_calls\":[{\"id\":\"a\",\"function\":{\"name\":\"bash\",\"arguments\":{\"command\":\"ls\"}}}]}}]}"
        `shouldBe` Right (Reply Nothing [ToolCall "a" "bash" "{\"command\":\"ls\"}"] mempty [])
    it "reports an error object returned with status 200" $
      replyFrom "{\"error\":{\"message\":\"rate limited\",\"code\":429}}"
        `shouldBe` Left "provider error: rate limited"
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
          Right complete <- newCompleteWith 1 (Provider OpenAICompatible ("http://127.0.0.1:" <> show port <> "/v1") Nothing Nothing)
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
    it "waits as long as Retry-After says on a 429" $ do
      r <- timeout 900000 (serve [RateLimited 0, ok])
      fmap (fmap replyText . fst . fst) r `shouldBe` Just (Right (Just "ok"))
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
      r `shouldBe` Right (Reply (Just "Hello") [] (Usage 3 1 0 Nothing) [])
      deltas `shouldBe` [TextDelta "Hel", TextDelta "lo"]
    it "fails a stream reset mid-body without retrying" $ do
      ((r, deltas), n) <- serve [Reset "data: {\"choices\":[{\"delta\":{\"content\":\"Hel\"}}]}\n\n", ok]
      r `shouldSatisfy` either (T.isPrefixOf "connection lost: ") (const False)
      deltas `shouldBe` [TextDelta "Hel"]
      n `shouldBe` 1
    it "fails a stream that stalls mid-body" $ do
      ((r, deltas), n) <- serve [Stall "text/event-stream" "data: {\"choices\":[{\"delta\":{\"content\":\"Hel\"}}]}\n\n", ok]
      r `shouldBe` Left "response stalled: no data for 1s"
      deltas `shouldBe` [TextDelta "Hel"]
      n `shouldBe` 1
    it "fails a plain JSON body that stalls" $ do
      ((r, _), n) <- serve [Stall "application/json" "{\"choi", ok]
      r `shouldBe` Left "response stalled: no data for 1s"
      n `shouldBe` 1
    it "fails a 200 body that is not JSON, without retrying" $ do
      ((r, _), n) <- serve [json 200 "not json", ok]
      r `shouldSatisfy` isLeft
      n `shouldBe` 1
    it "passes a plain JSON reply to the sink in one piece" $ do
      ((_, deltas), _) <- serve [ok]
      deltas `shouldBe` [TextDelta "ok"]

  describe "context windows" $ do
    let endpoints = "{\"data\":{\"id\":\"a/m\",\"endpoints\":[{\"context_length\":200000},{\"context_length\":128000},{\"context_length\":null}]}}"
        lookupAt port = lookupContext (Provider OpenRouter ("http://127.0.0.1:" <> show port <> "/api/v1") Nothing Nothing) "a/m"
    it "takes the smallest window among endpoints" $
      (endpointsContext =<< decode (BL.pack endpoints)) `shouldBe` Just 128000
    it "has none when no endpoint reports one" $
      (endpointsContext =<< decode "{\"data\":{\"endpoints\":[]}}") `shouldBe` Nothing
    it "looks the window up over HTTP" $ do
      port <- freePort
      (r, _) <- withServer port 0 [json 200 (BL.toStrict (BL.pack endpoints))] (lookupAt port)
      r `shouldBe` Just 128000
    it "has none for an unknown model or an unreachable server" $ do
      port <- freePort
      (r, _) <- withServer port 0 [json 404 "{}"] (lookupAt port)
      r `shouldBe` Nothing
      dead <- freePort
      lookupAt dead `shouldReturn` Nothing

  describe "newComplete" $
    it "rejects a base URL that does not parse" $ do
      r <- newComplete (Provider OpenAICompatible "not a url" Nothing Nothing)
      either Just (const Nothing) r `shouldBe` Just "invalid base URL: not a url"

  describe "parseKind" $
    it "round-trips every kind" $
      map (parseKind . kindName) [minBound .. maxBound] `shouldBe` map Just [minBound .. maxBound]
