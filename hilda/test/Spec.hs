module Main (main) where

import qualified AcpSpec
import qualified AppSpec
import qualified CliSpec
import qualified HeadlessSpec
import qualified MarkdownSpec
import qualified RenderSpec
import qualified ReplSpec
import qualified StateSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Acp" AcpSpec.spec
  describe "App" AppSpec.spec
  describe "Cli" CliSpec.spec
  describe "Headless" HeadlessSpec.spec
  describe "Markdown" MarkdownSpec.spec
  describe "Render" RenderSpec.spec
  describe "Repl" ReplSpec.spec
  describe "State" StateSpec.spec
