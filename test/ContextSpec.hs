module ContextSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Text as T
import Hilda.Context
import Hilda.Types
import Test.Hspec

big :: Int -> T.Text
big n = T.replicate n "x"

call :: T.Text -> ToolCall
call i = ToolCall i "read" "{}"

-- | Two old results, then a result the model has not seen yet.
history :: [Message]
history =
  [ System "sys"
  , User "go"
  , Assistant Nothing [call "a"] []
  , ToolResult "a" (big 4000)
  , Assistant Nothing [call "b"] []
  , ToolResult "b" (big 4000)
  , Assistant Nothing [call "c"] []
  , ToolResult "c" (big 4000)
  ]

results :: [Message] -> [T.Text]
results hist = [t | ToolResult _ t <- hist]

spec :: Spec
spec = do
  it "estimates four characters per token" $
    historyTokens [User (big 400), ToolResult "a" (big 400)] `shouldBe` 200

  it "leaves a history within budget alone" $
    fitContext 10000 history `shouldBe` (history, 0, 0)

  it "elides the oldest results first, down to three quarters of the budget" $ do
    let (hist, n, _) = fitContext 2900 history
    n `shouldBe` 1
    map T.length (results hist) `shouldSatisfy` \case
      [a, b, c] -> a < 100 && b == 4000 && c == 4000
      _ -> False

  it "never elides results after the last assistant message" $ do
    let (hist, n, _) = fitContext 1 history
    n `shouldBe` 2
    last (results hist) `shouldBe` big 4000

  it "keeps user and assistant messages" $ do
    let (hist, _, _) = fitContext 1 history
    [m | m <- hist, not (isResult m)] `shouldBe` [m | m <- history, not (isResult m)]

  it "trims further than the budget requires" $ do
    -- The history is about 3000 tokens. Fitting 2400 needs one result
    -- elided; fitting three quarters of it (1800) needs two.
    let (_, n, _) = fitContext 2400 history
    n `shouldBe` 2

  it "shrinks long arguments of old tool calls" $ do
    let write = ToolCall "w" "write" ("{\"path\":\"a.txt\",\"content\":\"" <> big 4000 <> "\"}")
        hist = [User "go", Assistant Nothing [write] [], ToolResult "w" "wrote", Assistant (Just "done") [] [], User "again", Assistant Nothing [call "r"] [], ToolResult "r" "x"]
        (out, n, _) = fitContext 100 hist
    n `shouldBe` 1
    [callArgs c | Assistant _ cs _ <- take 2 out, c <- cs]
      `shouldBe` ["{\"content\":\"[elided to fit the context budget: 4000 characters]\",\"path\":\"a.txt\"}"]

  it "keeps the arguments of the last assistant message" $ do
    let write = ToolCall "w" "write" ("{\"content\":\"" <> big 4000 <> "\"}")
        hist = [User "go", Assistant Nothing [write] [], ToolResult "w" "wrote"]
    fitContext 10 hist `shouldBe` (hist, 0, 0)

  it "counts reasoning toward the budget" $
    historyTokens [Assistant Nothing [] [String (big 400)]] `shouldBe` 101

  it "drops reasoning from earlier prompts' replies but keeps it in the current tool loop" $ do
    let r = String (big 4000)
        hist =
          [ User "a", Assistant (Just "x") [] [r]
          , User "b", Assistant Nothing [call "c"] [r], ToolResult "c" "ok", Assistant Nothing [call "d"] [r], ToolResult "d" "ok"
          ]
        (out, n, _) = fitContext 100 hist
    [rs | Assistant _ _ rs <- out] `shouldBe` [[], [r], [r]]
    n `shouldBe` 1

  it "is idempotent" $ do
    let (once, _, _) = fitContext 1 history
    fitContext 1 once `shouldBe` (once, 0, 0)

  it "reports the characters removed" $ do
    let (_, _, chars) = fitContext 2900 history
    chars `shouldBe` 4000 - T.length (elidedStub 4000)
  where
    isResult = \case
      ToolResult {} -> True
      _ -> False
