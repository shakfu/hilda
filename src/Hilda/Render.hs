-- | Human-readable rendering of agent events and usage, with optional ANSI
-- color. Rendering is pure; callers pick 'ansi' or 'plain'.
module Hilda.Render
  ( Color (..)
  , Paint
  , ansi
  , plain
  , colorEnabled
  , ansiTerminal
  , withStatus
  , Line (..)
  , renderEvent
  , renderUsage
  , formatCost
  , callSummary
  , estimateTokens
  , confirmDetail
  , confirmQuestion
  , isYes
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Char (isControl)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (encodeUtf8)
import Hilda.Agent (Event (..), resultLimit)
import Hilda.Tools (truncateMiddle)
import Hilda.Types
import System.Environment (lookupEnv)
import System.IO (Handle, hFlush, hIsTerminalDevice)
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

-- | A terminal that understands escape sequences: a tty, and TERM is not dumb.
ansiTerminal :: Handle -> IO Bool
ansiTerminal h = (&&) <$> hIsTerminalDevice h <*> ((/= Just "dumb") <$> lookupEnv "TERM")

-- | Color only on such a terminal, and not when NO_COLOR is set (no-color.org).
colorEnabled :: Handle -> IO Bool
colorEnabled h = do
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  (&& not noColor) <$> ansiTerminal h

-- | Run an action while a @[waiting Ns]@ line counts up on @h@, then erase
-- the line. The first update comes after one second, so fast calls print
-- nothing but the erase.
withStatus :: Handle -> Paint -> IO a -> IO a
withStatus h paint act = bracket (forkIO (tick 1)) stop (const act)
  where
    tick n = do
      threadDelay 1000000
      TIO.hPutStr h ("\r" <> paint Dim ("[waiting " <> tshow (n :: Int) <> "s]"))
      hFlush h
      tick (n + 1)
    stop t = killThread t >> TIO.hPutStr h "\r\ESC[K" >> hFlush h

-- | 'Partial' is printed without a newline; the next event completes it.
data Line = Partial Text | Full Text
  deriving stock (Eq, Show)

-- | One line per tool call: @[bash] git status -> ~40@ (estimated tokens).
renderEvent :: Paint -> Event -> Line
renderEvent paint = \case
  Narration t -> Full (paint Dim (T.strip t))
  CallStarted c -> Partial (paint Cyan ("[" <> callName c <> "]") <> " " <> callSummary c <> " ")
  CallFinished _ (Right r) -> Full (paint Dim ("-> ~" <> tshow (estimateTokens r)))
  CallFinished _ (Left e) -> Full (paint Red ("-> error: " <> elide 100 e))

-- | The argument that identifies the call (command or path), else the raw
-- arguments; one line, at most 80 characters.
callSummary :: ToolCall -> Text
callSummary c = elide 80 . visible $ (fromMaybe (callArgs c) (primary =<< decodeStrict (encodeUtf8 (callArgs c))))
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

-- | Every argument of a call, unshortened, for the user to approve.
-- File content shows as a size; control characters show escaped, so a
-- carriage return or escape sequence cannot hide part of a command.
confirmDetail :: ToolCall -> Text
confirmDetail c = case decodeStrict (encodeUtf8 (callArgs c)) of
  Just (Object o) ->
    T.intercalate "\n" [field k v | (k, v) <- ordered (KM.toList o)]
  _ -> field "arguments" (String (callArgs c))
  where
    ordered kvs =
      [kv | k <- first', Just kv <- [(,) k <$> lookup k kvs]] <> [kv | kv@(k, _) <- kvs, k `notElem` first']
    first' = ["path", "command", "old_string", "new_string"]
    field k v = "  " <> Key.toText k <> ": " <> case (k, v) of
      ("content", String s) -> "<" <> tshow (T.length s) <> " characters>"
      (_, String s) -> indent (visible s)
      _ -> visible (TL.toStrict (encodeToLazyText v))
    indent = T.intercalate "\n    " . T.splitOn "\n"

confirmQuestion :: ToolCall -> Text
confirmQuestion c = "allow [" <> callName c <> "]? [y/N] "

-- | Escape control characters other than newline and tab.
visible :: Text -> Text
visible = T.concatMap $ \ch ->
  if isControl ch && ch /= '\n' && ch /= '\t'
    then T.pack (printf "\\x%02x" (fromEnum ch))
    else T.singleton ch

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
