module ToolsSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Hilda.Tools
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

bash :: String -> Int -> IO (Either T.Text T.Text)
bash cmd secs = toolRun bashTool (object ["command" .= cmd, "timeout" .= secs])

spec :: Spec
spec = do
  describe "applyEdit" $ do
    it "replaces a unique match" $
      applyEdit "b" "x" False "abc" `shouldBe` Right "axc"
    it "refuses a missing match" $
      applyEdit "z" "x" False "abc" `shouldBe` Left "old_string not found"
    it "refuses an ambiguous match unless replace_all" $ do
      applyEdit "a" "x" False "aba" `shouldSatisfy` either (T.isInfixOf "2 times") (const False)
      applyEdit "a" "x" True "aba" `shouldBe` Right "xbx"
    it "refuses an empty or unchanged edit" $ do
      applyEdit "" "x" False "abc" `shouldBe` Left "old_string is empty"
      applyEdit "a" "a" False "abc" `shouldBe` Left "old_string and new_string are identical"

  describe "numberLines" $ do
    let ls = ["a", "b", "c", "d"]
    it "numbers from the offset and names where to continue" $
      numberLines 1000 2 2 ls `shouldBe` "2\tb\n3\tc\n[more lines follow; continue with offset=4]\n"
    it "adds no continuation at the end of the file" $
      numberLines 1000 3 10 ls `shouldBe` "3\tc\n4\td\n"
    it "stops at the character budget" $
      numberLines 8 1 10 ls `shouldBe` "1\ta\n2\tb\n[more lines follow; continue with offset=3]\n"
    it "cuts a single line longer than the budget" $
      numberLines 5 1 10 ["abcdefgh", "x"]
        `shouldBe` "1\tabc\n[line 1 cut to 5 characters]\n[more lines follow; continue with offset=2]\n"
    it "reports an offset past the end" $
      numberLines 1000 9 5 ls `shouldBe` "(no lines at offset 9; the file is shorter)"

  describe "truncateMiddle" $ do
    it "keeps short text" $ truncateMiddle 10 "abc" `shouldBe` "abc"
    it "keeps head and tail of long text" $ do
      let t = truncateMiddle 10 (T.replicate 100 "x" <> "END")
      t `shouldSatisfy` T.isSuffixOf "END"
      t `shouldSatisfy` T.isInfixOf "93 characters omitted"

  describe "file tools" $ do
    it "writes, reads and edits a file" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "sub" </> "f.txt"
      toolRun writeTool (object ["path" .= path, "content" .= ("one\ntwo\n" :: String)])
        `shouldReturn` Right ("wrote 8 bytes to " <> T.pack path)
      toolRun editTool (object ["path" .= path, "old_string" .= ("two" :: String), "new_string" .= ("2" :: String)])
        `shouldReturn` Right ("edited " <> T.pack path)
      toolRun readTool (object ["path" .= path]) `shouldReturn` Right "1\tone\n2\t2\n"

    it "leaves the file untouched when an edit fails" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "f.txt"
      writeFile path "aa"
      r <- toolRun editTool (object ["path" .= path, "old_string" .= ("a" :: String), "new_string" .= ("b" :: String)])
      r `shouldSatisfy` either (const True) (const False)
      readFile path `shouldReturn` "aa"

    it "keeps permissions and symlinks when editing" $ withSystemTempDirectory "hilda" $ \dir -> do
      let real = dir </> "script.sh"
          link = dir </> "link.sh"
      writeFile real "echo a"
      getPermissions real >>= setPermissions real . setOwnerExecutable True
      createFileLink real link
      _ <- toolRun editTool (object ["path" .= link, "old_string" .= ("a" :: String), "new_string" .= ("b" :: String)])
      pathIsSymbolicLink link `shouldReturn` True
      readFile real `shouldReturn` "echo b"
      executable <$> getPermissions real `shouldReturn` True

    it "pages a large file under the result limit" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "big.txt"
      writeFile path (unlines (replicate 100000 (replicate 50 'x')))
      Right out <- toolRun readTool (object ["path" .= path])
      T.length out `shouldSatisfy` (< resultLimit)
      out `shouldSatisfy` T.isInfixOf "continue with offset="

    it "refuses to edit files over the size limit" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "huge.txt"
      writeFile path (replicate (editLimit + 1) 'a')
      r <- toolRun editTool (object ["path" .= path, "old_string" .= ("a" :: String), "new_string" .= ("b" :: String), "replace_all" .= True])
      r `shouldSatisfy` either (T.isInfixOf "use bash") (const False)

    it "writes through a fresh temporary file" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "f.txt"
          victim = dir </> "victim.txt"
      writeFile victim "safe"
      -- The old fixed temporary name, planted as a symlink.
      createFileLink victim (path <> ".hilda-tmp")
      atomicWrite path "new"
      readFile path `shouldReturn` "new"
      readFile victim `shouldReturn` "safe"
      listDirectory dir >>= (`shouldMatchList` ["f.txt", "f.txt.hilda-tmp", "victim.txt"])

    it "gives new files default permissions" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "new.txt"
      atomicWrite path "x"
      p <- getPermissions path
      (readable p, writable p, executable p) `shouldBe` (True, True, False)

    it "refuses to read binary files" $ withSystemTempDirectory "hilda" $ \dir -> do
      let path = dir </> "bin"
      writeFile path "a\0b"
      r <- toolRun readTool (object ["path" .= path])
      r `shouldSatisfy` either (T.isInfixOf "binary") (const False)

  describe "bash" $ do
    it "returns combined output and the exit code" $
      bash "echo out; echo err >&2; exit 3" 10 `shouldReturn` Right "out\nerr\n[exit code 3]"

    it "kills the whole process group on timeout" $ withSystemTempDirectory "hilda" $ \dir -> do
      let marker = dir </> "marker"
      r <- timeout 10000000 (bash ("(sleep 2; touch " <> marker <> ") & sleep 30") 1)
      r `shouldBe` Just (Left "timed out after 1s")
      threadDelay 2500000
      doesFileExist marker `shouldReturn` False

    it "caps captured output" $ do
      r <- bash "head -c 3000000 /dev/zero | tr '\\0' x" 30
      either (const 0) T.length r `shouldSatisfy` (\n -> n > 1000000 && n < 1100000)
