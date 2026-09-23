module ProviderSpec (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (isLeft)
import qualified Data.Text as T
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
      keys (encodeRequest (Request "m" [User "hi"] [])) `shouldMatchList` ["model", "messages", "stream", "stream_options"]
    it "sends tools with tool_choice auto" $ do
      let v = encodeRequest (Request "m" [] [object []])
      keys v `shouldMatchList` ["model", "messages", "stream", "stream_options", "tools", "tool_choice"]

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
        `shouldBe` Right (Reply (Just "hi") [] (Usage 3 4 Nothing))
    it "reads OpenRouter's cost" $
      fmap replyUsage (replyFrom "{\"choices\":[{\"message\":{\"content\":\"hi\"}}],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":4,\"cost\":0.0021}}")
        `shouldBe` Right (Usage 3 4 (Just 0.0021))
    it "adds costs where reported" $ do
      Usage 1 2 (Just 0.5) <> Usage 3 4 Nothing `shouldBe` Usage 4 6 (Just 0.5)
      Usage 1 2 Nothing <> Usage 3 4 Nothing `shouldBe` Usage 4 6 Nothing
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

  describe "parseKind" $
    it "round-trips every kind" $
      map (parseKind . kindName) [minBound .. maxBound] `shouldBe` map Just [minBound .. maxBound]
