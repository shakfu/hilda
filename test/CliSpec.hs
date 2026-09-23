module CliSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Hilda.App (Output (..), versionText)
import Hilda.Cli
import Hilda.Policy (Mode (..))
import Hilda.Provider
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, renderFailure)
import Test.Hspec

parse :: [String] -> Maybe Options
parse args = case execParserPure defaultPrefs optionsInfo args of
  Success o -> Just o
  _         -> Nothing

-- | Resolve with an environment and remembered models.
resolve :: [(String, String)] -> [(ProviderKind, Text)] -> [String] -> Either Text (Provider, Text)
resolve env models args = case parse args of
  Nothing -> Left "parse failed"
  Just o -> resolveProvider (`lookup` env) (`lookup` models) o

spec :: Spec
spec = do
  describe "options" $ do
    it "parses a headless json run" $ do
      let o = parse ["-p", "hi", "--json", "--mode", "read-only", "--system", "be brief"]
      fmap optPrompt o `shouldBe` Just (Just "hi")
      fmap optOutput o `shouldBe` Just Json
      fmap optMode o `shouldBe` Just ReadOnly
      fmap optSystem o `shouldBe` Just (Just (SystemText "be brief"))
    it "parses --stream-json" $
      fmap optOutput (parse ["-p", "hi", "--stream-json"]) `shouldBe` Just StreamJson
    it "rejects --json with --stream-json" $
      parse ["-p", "hi", "--json", "--stream-json"] `shouldBe` Nothing
    it "defaults to text output, yolo and AGENTS.md discovery" $ do
      fmap optOutput (parse []) `shouldBe` Just Text
      fmap optMode (parse []) `shouldBe` Just Yolo
      fmap optAgents (parse []) `shouldBe` Just Discover
    it "collects repeated --agents" $
      fmap optAgents (parse ["--agents", "a.md", "--agents", "b.md"]) `shouldBe` Just (Explicit ["a.md", "b.md"])
    it "rejects --system with --system-file" $
      parse ["--system", "x", "--system-file", "y"] `shouldBe` Nothing
    it "prints the version and exits" $
      case execParserPure defaultPrefs optionsInfo ["--version"] of
        Failure f -> fst (renderFailure f "hilda") `shouldBe` T.unpack versionText
        _ -> expectationFailure "--version did not exit"
    it "rejects an unknown mode" $
      parse ["--mode", "sudo"] `shouldBe` Nothing

  describe "resolveProvider" $ do
    let orKey = [("OPENROUTER_API_KEY", "k")]
    it "uses OpenRouter when its key is set" $
      resolve orKey [] ["-m", "a/b"]
        `shouldBe` Right (Provider OpenRouter "https://openrouter.ai/api/v1" (Just "k"), "a/b")
    it "reuses the last model for the provider" $
      fmap snd (resolve orKey [(OpenRouter, "a/b"), (OpenAICompatible, "local")] []) `shouldBe` Right "a/b"
    it "prefers -m over the remembered model" $
      fmap snd (resolve orKey [(OpenRouter, "a/b")] ["-m", "c/d"]) `shouldBe` Right "c/d"
    it "fails without -m or a remembered model" $
      resolve orKey [] [] `shouldSatisfy` isLeft
    it "lets -P override the OpenRouter default" $
      resolve orKey [(OpenAICompatible, "local")] ["-P", "openai"]
        `shouldBe` Right (Provider OpenAICompatible "https://api.openai.com/v1" Nothing, "local")
    it "defaults to openai without an OpenRouter key" $
      fmap (providerKind . fst) (resolve [] [] ["-m", "x"]) `shouldBe` Right OpenAICompatible
    it "requires a key for OpenRouter" $
      resolve [] [] ["-P", "openrouter", "-m", "x"] `shouldSatisfy` isLeft
    it "prefers --base-url over OPENAI_BASE_URL over the default" $ do
      let env = [("OPENAI_BASE_URL", "http://env/v1")]
      fmap (providerBaseUrl . fst) (resolve env [] ["-m", "x"]) `shouldBe` Right "http://env/v1"
      fmap (providerBaseUrl . fst) (resolve env [] ["-m", "x", "--base-url", "http://flag/v1"])
        `shouldBe` Right "http://flag/v1"
    it "fails when --api-key-env names an unset variable" $
      resolve [] [] ["-m", "x", "--api-key-env", "MY_KEY"] `shouldSatisfy` isLeft
    it "rejects an unknown provider" $
      resolve [] [] ["-m", "x", "-P", "acme"] `shouldSatisfy` isLeft
