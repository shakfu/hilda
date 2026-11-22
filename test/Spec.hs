module Main (main) where

import qualified AcpSpec
import qualified AgentSpec
import qualified AppSpec
import qualified CliSpec
import qualified ContextSpec
import qualified FamilySpec
import qualified HeadlessSpec
import qualified MarkdownSpec
import qualified PolicySpec
import qualified PromptSpec
import qualified ProviderSpec
import qualified RenderSpec
import qualified ReplSpec
import qualified StateSpec
import qualified StreamSpec
import qualified ToolsSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Acp" AcpSpec.spec
  describe "Agent" AgentSpec.spec
  describe "App" AppSpec.spec
  describe "Cli" CliSpec.spec
  describe "Context" ContextSpec.spec
  describe "Family" FamilySpec.spec
  describe "Headless" HeadlessSpec.spec
  describe "Markdown" MarkdownSpec.spec
  describe "Policy" PolicySpec.spec
  describe "Prompt" PromptSpec.spec
  describe "Provider" ProviderSpec.spec
  describe "Render" RenderSpec.spec
  describe "Repl" ReplSpec.spec
  describe "State" StateSpec.spec
  describe "Stream" StreamSpec.spec
  describe "Tools" ToolsSpec.spec
