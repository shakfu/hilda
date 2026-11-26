module Main (main) where

import qualified AgentSpec
import qualified ContextSpec
import qualified FamilySpec
import qualified PolicySpec
import qualified PromptSpec
import qualified ProviderSpec
import qualified StreamSpec
import qualified ToolsSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Agent" AgentSpec.spec
  describe "Context" ContextSpec.spec
  describe "Family" FamilySpec.spec
  describe "Policy" PolicySpec.spec
  describe "Prompt" PromptSpec.spec
  describe "Provider" ProviderSpec.spec
  describe "Stream" StreamSpec.spec
  describe "Tools" ToolsSpec.spec
