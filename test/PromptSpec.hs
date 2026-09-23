module PromptSpec (spec) where

import Hilda.Prompt
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "discoverAgents" $ do
    it "collects AGENTS.md from the git root down to cwd" $ withSystemTempDirectory "hilda" $ \root -> do
      let cwd = root </> "a" </> "b"
      createDirectory (root </> ".git")
      createDirectoryIfMissing True cwd
      writeFile (root </> "AGENTS.md") "root"
      writeFile (root </> "a" </> "AGENTS.md") "a"
      discoverAgents cwd `shouldReturn` [root </> "AGENTS.md", root </> "a" </> "AGENTS.md"]

    it "reads only cwd outside a repository" $ withSystemTempDirectory "hilda" $ \root -> do
      let cwd = root </> "a"
      createDirectoryIfMissing True cwd
      writeFile (root </> "AGENTS.md") "ignored"
      writeFile (cwd </> "AGENTS.md") "used"
      discoverAgents cwd `shouldReturn` [cwd </> "AGENTS.md"]

  describe "assemble" $ do
    it "orders instructions, working directory, then AGENTS.md files" $
      assemble ["BASE\n", "EXTRA"] "/w" [("/w/AGENTS.md", "rules\n")]
        `shouldBe` "BASE\n\nEXTRA\n\nWorking directory: /w\n\nInstructions from /w/AGENTS.md:\n\nrules"
    it "drops blank instruction blocks" $
      assemble ["BASE", "  "] "/w" [] `shouldBe` "BASE\n\nWorking directory: /w"
