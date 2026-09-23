-- | Keeping the conversation within a token budget.
--
-- Tool results make up most of a coding session's history and can be
-- fetched again, so they are elided oldest first. User and assistant text
-- is kept. Elision is written into the history rather than applied per
-- request, so the sent prefix stays stable for provider prompt caching.
module Hilda.Context
  ( historyTokens
  , fitContext
  , elidedStub
  ) where

import qualified Data.IntSet as IS
import Data.Text (Text)
import qualified Data.Text as T
import Hilda.Types

-- | Estimated tokens of a history as sent, at four characters per token.
historyTokens :: [Message] -> Int
historyTokens hist = (sum (map chars hist) + 3) `div` 4
  where
    chars = \case
      System t -> T.length t
      User t -> T.length t
      Assistant t calls -> maybe 0 T.length t + sum [T.length (callName c) + T.length (callArgs c) | c <- calls]
      ToolResult _ t -> T.length t

elidedStub :: Int -> Text
elidedStub n = "[elided to fit the context budget: " <> T.pack (show n) <> " characters; run the tool again if needed]"

-- | Once over @budget@ tokens, elide the oldest tool results until the
-- history fits three quarters of it. Trimming past the budget makes trims
-- rarer; each one changes an early message and invalidates the provider's
-- prompt cache from there on.
--
-- Results after the last assistant message, which the model has not seen,
-- are kept. Returns the history, the number of results elided and the
-- characters removed. A history that still does not fit is returned as is.
fitContext :: Int -> [Message] -> ([Message], Int, Int)
fitContext budget hist
  | historyTokens hist <= budget || null chosen = (hist, 0, 0)
  | otherwise = (zipWith elide [0 ..] hist, length chosen, sum (map snd chosen))
  where
    need = 4 * (historyTokens hist - budget * 3 `div` 4)
    lastAssistant = maximum (-1 : [i | (i, Assistant {}) <- indexed])
    indexed = zip [0 :: Int ..] hist
    -- (index, characters saved), oldest first.
    candidates =
      [ (i, saved)
      | (i, ToolResult _ t) <- indexed
      , i < lastAssistant
      , not (elidedPrefix `T.isPrefixOf` t)
      , let saved = T.length t - T.length (elidedStub (T.length t))
      , saved > 0
      ]
    chosen = cover 0 candidates
    cover _ [] = []
    cover acc (c@(_, saved) : cs)
      | acc >= need = []
      | otherwise = c : cover (acc + saved) cs
    picked = IS.fromList (map fst chosen)
    elide i = \case
      ToolResult cid t | i `IS.member` picked -> ToolResult cid (elidedStub (T.length t))
      m -> m
    elidedPrefix = "[elided to fit the context budget"
