module MarkdownSpec (spec) where

import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Text as T
import Hilda.Markdown
import Test.Hspec

-- | Render input that arrives in the given pieces.
render :: [Text] -> Text
render pieces =
  let (st, outs) = mapAccumL (\s p -> let (o, s') = feedMarkdown p s in (s', o)) markdown pieces
   in T.concat outs <> endMarkdown st

whole, perChar :: Text -> Text
whole = renderMarkdown
perChar = render . T.chunksOf 1

spec :: Spec
spec = do
  it "styles bold, italic and code spans" $ do
    whole "a **b** c" `shouldBe` "a \ESC[0m\ESC[1mb\ESC[0m c"
    whole "a *b* c" `shouldBe` "a \ESC[0m\ESC[3mb\ESC[0m c"
    whole "run `ls -l` now" `shouldBe` "run \ESC[0m\ESC[36mls -l\ESC[0m now"
  it "styles headings without the hashes" $
    whole "## Plan\nnext" `shouldBe` "\ESC[0m\ESC[1;35mPlan\ESC[0m\nnext"
  it "leaves code blocks unstyled inside and dims the fences" $
    whole "```sh\nrm *.o **x**\n```\nok" `shouldBe`
      "\ESC[0m\ESC[2m```sh\ESC[0m\nrm *.o **x**\n\ESC[0m\ESC[2m```\ESC[0m\nok"
  it "does not close a block on a fence with trailing text" $
    whole "```\n```x\n```\n**b**" `shouldBe`
      "\ESC[0m\ESC[2m```\ESC[0m\n```x\n\ESC[0m\ESC[2m```\ESC[0m\n\ESC[0m\ESC[1mb\ESC[0m"
  it "keeps markers that are not emphasis" $ do
    whole "2 * 3 * 4" `shouldBe` "2 * 3 * 4"
    whole "snake_case_name" `shouldBe` "snake_case_name"
    whole "* item\n- item\n***" `shouldBe` "* item\n- item\n***"
    whole "#tag and \\*literal\\*" `shouldBe` "#tag and *literal*"
  it "ends styles at the newline" $
    whole "**open\nplain" `shouldBe` "\ESC[0m\ESC[1mopen\ESC[0m\nplain"
  it "treats code span content literally" $
    whole "`a*b*c`" `shouldBe` "\ESC[0m\ESC[36ma*b*c\ESC[0m"
  it "renders the same however the input is split" $ do
    let doc = "# Title\n\nSome **bold**, *em* and `code`.\n\n* a\n* b\n\n```hs\nx = 2 * 3\n```\n~~~\nno *style*\n~~~\nend_of_it *"
    perChar doc `shouldBe` whole doc
    render (T.chunksOf 3 doc) `shouldBe` whole doc
  it "holds back only undecided input" $ do
    fst (feedMarkdown "plain text" markdown) `shouldBe` "plain text"
    fst (feedMarkdown "wait *" markdown) `shouldBe` "wait "
