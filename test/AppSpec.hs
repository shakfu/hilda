module AppSpec (spec) where

import Data.Aeson (Value (..), (.:))
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe, withObject)
import Data.Text (Text)
import Hilda.Agent
import Hilda.App
import Hilda.Policy (Mode (..))
import Hilda.Provider (ProviderKind (..))
import Hilda.Types
import System.Exit (ExitCode (..))
import Test.Hspec

field :: Text -> Value -> Maybe Value
field k = parseMaybe (withObject "event" (.: Key.fromText k))

cfg :: Config
cfg = Config (\_ _ -> pure (Left "unused")) OpenRouter "m" Yolo "sys" 5 (const (pure 1000)) Nothing (const (pure ()))

spec :: Spec
spec = do
  describe "eventJson" $ do
    let c = ToolCall "c1" "bash" "{}"
    it "tags each event type" $
      map (field "type" . eventJson) [Narration "x", CallStarted c, CallFinished c (Right "out")]
        `shouldBe` map (Just . String) ["text", "tool_call", "tool_result"]
    it "reports context trimming" $
      field "type" (eventJson (ContextTrimmed 2 900)) `shouldBe` Just (String "context_trimmed")
    it "reports dropped reasoning" $
      field "type" (eventJson ReasoningDropped) `shouldBe` Just (String "reasoning_dropped")
    it "marks failed tool results" $
      field "ok" (eventJson (CallFinished c (Left "boom"))) `shouldBe` Just (Bool False)

  describe "deltaJson" $
    it "tags streamed text and reasoning" $ do
      field "type" (deltaJson (TextDelta "hi")) `shouldBe` Just (String "text_delta")
      field "type" (deltaJson (ReasoningDelta "hmm")) `shouldBe` Just (String "reasoning_delta")

  describe "outcomeJson" $
    it "tags the result line and reports the stop reason" $ do
      let v = outcomeJson cfg (Outcome [] "done" 1 mempty 42 Finished)
      field "type" v `shouldBe` Just (String "result")
      field "result" v `shouldBe` Just (String "done")
      field "stop" v `shouldBe` Just (String "finished")
      field "context_tokens" v `shouldBe` Just (Number 42)

  describe "exitCodeFor" $
    it "separates errors from the turn limit" $
      map exitCodeFor [Finished, Failed "x", TurnLimit, CostLimit] `shouldBe` [ExitSuccess, ExitFailure 1, ExitFailure 2, ExitFailure 3]
