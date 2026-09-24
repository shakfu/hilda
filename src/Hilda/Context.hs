-- | Keeping the conversation within a token budget.
--
-- Tool results, long tool-call arguments (file contents sent to
-- @write@ or @edit@) and earlier prompts' reasoning make up most of a coding session's history and can be
-- recovered from disk, so they are elided oldest first. User and assistant
-- text is kept. Elision is written into the history rather than applied per
-- request, so the sent prefix stays stable for provider prompt caching.
module Hilda.Context
  ( historyTokens
  , fitContext
  , elidedStub
  ) where

import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.IntSet as IS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import Hilda.Types

-- | Estimated tokens of a history as sent, at four characters per token.
historyTokens :: [Message] -> Int
historyTokens hist = (sum (map chars hist) + 3) `div` 4
  where
    chars = \case
      System t -> T.length t
      User t -> T.length t
      Assistant t calls rs -> maybe 0 T.length t + sum [T.length (callName c) + T.length (callArgs c) | c <- calls] + reasoningChars rs
      ToolResult _ t -> T.length t

-- | Replacement for a tool result of @n@ characters.
elidedStub :: Int -> Text
elidedStub n = "[elided to fit the context budget: " <> T.pack (show n) <> " characters; run the tool again if needed]"

-- | Once over @budget@ tokens, elide the oldest tool results and long
-- tool-call arguments until the
-- history fits three quarters of it. Trimming past the budget makes trims
-- rarer; each one changes an early message and invalidates the provider's
-- prompt cache from there on.
--
-- The last assistant message and the results after it are kept. Returns
-- the history, the number of messages elided and the characters removed. A history that still does not fit is returned as is.
fitContext :: Int -> [Message] -> ([Message], Int, Int)
fitContext budget hist
  | historyTokens hist <= budget || null chosen = (hist, 0, 0)
  | otherwise = (zipWith elide [0 ..] hist, length chosen, sum (map snd chosen))
  where
    need = 4 * (historyTokens hist - budget * 3 `div` 4)
    lastAssistant = maximum (-1 : [i | (i, Assistant {}) <- indexed])
    -- Reasoning is dropped only from earlier prompts' replies: providers
    -- need it unchanged within the current tool loop.
    lastUser = maximum (-1 : [i | (i, User _) <- indexed])
    indexed = zip [0 :: Int ..] hist
    -- (index, characters saved), oldest first.
    candidates = [(i, saved) | (i, m) <- indexed, i < lastAssistant, let saved = savedBy i m, saved > 0]
    savedBy i = \case
      ToolResult _ t
        | elidedPrefix `T.isPrefixOf` t -> 0
        | otherwise -> T.length t - T.length (elidedStub (T.length t))
      Assistant _ calls rs -> sum (map (snd . shrinkArgs . callArgs) calls) + (if i < lastUser then reasoningChars rs else 0)
      _ -> 0
    chosen = cover 0 candidates
    cover _ [] = []
    cover acc (c@(_, saved) : cs)
      | acc >= need = []
      | otherwise = c : cover (acc + saved) cs
    picked = IS.fromList (map fst chosen)
    elide i = \case
      ToolResult cid t | i `IS.member` picked -> ToolResult cid (elidedStub (T.length t))
      Assistant t calls rs | i `IS.member` picked ->
        Assistant t [c {callArgs = fst (shrinkArgs (callArgs c))} | c <- calls] (if i < lastUser then [] else rs)
      m -> m

-- | Characters of reasoning_details blocks as sent, in JSON.
reasoningChars :: [Value] -> Int
reasoningChars = sum . map (fromIntegral . TL.length . encodeToLazyText)

-- | Start of every stub, so elided text is never elided again.
elidedPrefix :: Text
elidedPrefix = "[elided to fit the context budget"

-- | Tool-call arguments with every top-level string over 300 characters
-- replaced by a stub, re-encoded as JSON; and the characters saved.
-- Arguments that are not a JSON object, or have nothing to shrink, are
-- unchanged.
shrinkArgs :: Text -> (Text, Int)
shrinkArgs raw = case decodeStrict (encodeUtf8 raw) of
  Just (Object o) | any long (KM.elems o) ->
    let new = TL.toStrict (encodeToLazyText (Object (KM.map shrink o)))
     in if T.length new < T.length raw then (new, T.length raw - T.length new) else (raw, 0)
  _ -> (raw, 0)
  where
    long = \case
      String s -> T.length s > 300 && not (elidedPrefix `T.isPrefixOf` s)
      _ -> False
    shrink v = case v of
      String s | long v -> String (elidedPrefix <> ": " <> T.pack (show (T.length s)) <> " characters]")
      _ -> v
