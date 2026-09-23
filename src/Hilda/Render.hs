-- | Human-readable rendering of agent events and usage, with optional ANSI
-- color. Rendering is pure; callers pick 'ansi' or 'plain'.
module Hilda.Render
  ( Color (..)
  , Paint
  , ansi
  , plain
  , colorEnabled
  , Line (..)
  , renderEvent
  , renderUsage
  , formatCost
  , callSummary
  , estimateTokens
  , confirmQuestion
  , isYes
  ) where

import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Hilda.Agent (Event (..), resultLimit)
import Hilda.Tools (truncateMiddle)
import Hilda.Types
import System.Environment (lookupEnv)
import System.IO (Handle, hIsTerminalDevice)
import Text.Printf (printf)

data Color = Dim | Red | Cyan | BoldMagenta
  deriving stock (Eq, Show)

type Paint = Color -> Text -> Text

ansi :: Paint
ansi c t = "\ESC[" <> code c <> "m" <> t <> "\ESC[0m"
  where
    code = \case
      Dim  -> "2"
      Red  -> "31"
      Cyan -> "36"
      BoldMagenta -> "1;35"

plain :: Paint
plain _ t = t

-- | Color only on a terminal, and not when NO_COLOR is set (no-color.org)
-- or TERM is dumb.
colorEnabled :: Handle -> IO Bool
colorEnabled h = do
  tty <- hIsTerminalDevice h
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  term <- lookupEnv "TERM"
  pure (tty && not noColor && term /= Just "dumb")

-- | 'Partial' is printed without a newline; the next event completes it.
data Line = Partial Text | Full Text
  deriving stock (Eq, Show)

-- | One line per tool call: @[bash] git status -> ~40 tokens@.
renderEvent :: Paint -> Event -> Line
renderEvent paint = \case
  Narration t -> Full (paint Dim (T.strip t))
  CallStarted c -> Partial (paint Cyan ("[" <> callName c <> "]") <> " " <> callSummary c <> " ")
  CallFinished _ (Right r) -> Full (paint Dim ("-> ~" <> tshow (estimateTokens r) <> " tokens"))
  CallFinished _ (Left e) -> Full (paint Red ("-> error: " <> elide 100 e))

-- | The argument that identifies the call (command or path), else the raw
-- arguments; one line, at most 80 characters.
callSummary :: ToolCall -> Text
callSummary c = elide 80 (fromMaybe (callArgs c) (primary =<< decodeStrict (encodeUtf8 (callArgs c))))
  where
    primary = \case
      Object o -> listToMaybe [s | k <- ["command", "path"], Just (String s) <- [KM.lookup k o]]
      _ -> Nothing

-- | Rough token count of a tool result as the model receives it, at four
-- characters per token. No tokenizer is available for arbitrary models.
estimateTokens :: Text -> Int
estimateTokens r = (T.length (truncateMiddle resultLimit r) + 3) `div` 4

renderUsage :: Usage -> Text
renderUsage u =
  tshow (usagePrompt u) <> " in / " <> tshow (usageCompletion u) <> " out"
    <> maybe "" ((", " <>) . formatCost) (usageCost u)

-- | Six decimals below one cent, otherwise four.
formatCost :: Double -> Text
formatCost x = T.pack (printf (if x < 0.01 then "$%.6f" else "$%.4f") x)

confirmQuestion :: ToolCall -> Text
confirmQuestion c = "allow [" <> callName c <> "] " <> callSummary c <> "? [y/N] "

isYes :: Text -> Bool
isYes t = T.toLower (T.strip t) `elem` ["y", "yes"]

-- | Collapse whitespace to one line and cut at @n@ characters with "..".
elide :: Int -> Text -> Text
elide n t
  | T.length flat > n = T.take (n - 2) flat <> ".."
  | otherwise = flat
  where
    flat = T.unwords (T.words t)

tshow :: Show a => a -> Text
tshow = T.pack . show
