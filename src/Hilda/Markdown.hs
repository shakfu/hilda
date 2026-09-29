-- | Terminal styling of markdown as it streams. Handles headings, bold,
-- italic, code spans and fenced code; other syntax prints verbatim.
--
-- Input is styled as it arrives, holding back only the few characters
-- whose meaning depends on the next one (a @*@ that may start @**@, a
-- line that may be a fence). Styles end at each newline, so an unclosed
-- marker affects one line at most.
module Hilda.Markdown
  ( Markdown
  , markdown
  , feedMarkdown
  , endMarkdown
  , renderMarkdown
  ) where

import Data.Char (isAlphaNum, isPunctuation, isSpace, isSymbol)
import Data.Text (Text)
import qualified Data.Text as T

-- | What the rest of the current line is.
data LineKind = Prose | Heading | Fence | Code
  deriving stock (Eq, Show)

-- | Renderer state between pieces of input.
data Markdown = Markdown
  { mdPending   :: String -- ^ Input held until the next piece decides it.
  , mdLineStart :: Bool
  , mdLine      :: LineKind
  , mdFence     :: Maybe String -- ^ Opening fence of the enclosing code block.
  , mdStrong    :: Bool
  , mdEm        :: Bool
  , mdSpan      :: Int -- ^ Backtick count of the open code span; 0 when none.
  , mdPrev      :: Char -- ^ Last character consumed, for emphasis rules.
  }

-- | State at the start of a reply.
markdown :: Markdown
markdown = Markdown "" True Prose Nothing False False 0 ' '

-- | Style the next piece of input; returns what can be printed now.
feedMarkdown :: Text -> Markdown -> (Text, Markdown)
feedMarkdown = feed False

-- | Print held input and reset the terminal style.
endMarkdown :: Markdown -> Text
endMarkdown st = let (out, st') = feed True "" st in out <> reset st'

-- | Style a complete text.
renderMarkdown :: Text -> Text
renderMarkdown t = let (out, st) = feedMarkdown t markdown in out <> endMarkdown st

feed :: Bool -> Text -> Markdown -> (Text, Markdown)
feed eof t st0 = go st0 (mdPending st0 <> T.unpack t) []
  where
    go st [] acc = (T.concat (reverse acc), st {mdPending = ""})
    go st xs acc = case step eof st xs of
      Nothing -> (T.concat (reverse acc), st {mdPending = xs})
      Just (out, st', rest) -> go st' rest (out : acc)

-- | Consume a prefix of the input, or 'Nothing' to wait for more.
-- At end of input (@eof@) every case decides.
step :: Bool -> Markdown -> String -> Maybe (Text, Markdown, String)
step eof st s
  | mdLineStart st = lineStart eof st s
  | otherwise = inline eof st s

lineStart :: Bool -> Markdown -> String -> Maybe (Text, Markdown, String)
lineStart eof st s = case rest of
  [] | eof -> emit ws [] st
     | otherwise -> Nothing
  '\n' : _ -> emit ws rest st
  c : _
    | Just f <- mdFence st -> inFence f c
    | c `elem` ("`~" :: String) -> fenceOpen c
    | c == '#' -> heading
    | c `elem` ("*_" :: String), all (`elem` [c, ' ']) line -> rule c
    | c `elem` ("*-+" :: String) -> bullet c
    | otherwise -> emit ws rest st
  where
    (ws, rest) = span (`elem` [' ', '\t']) s
    (line, afterLine) = break (== '\n') rest
    lineDone = eof || not (null afterLine)
    emit out r st' = Just (T.pack out, st' {mdLineStart = False}, r)
    styled st' out r = Just (restyle st st' <> T.pack out, st' {mdLineStart = False}, r)

    -- Inside a block, only a fence line of the same character, at least as
    -- long, with nothing after it, closes the block.
    inFence f c
      | take 1 f /= [c] = styled st {mdLine = Code} ws rest
      | not lineDone && all isSpace after = Nothing
      | length run >= length f && all isSpace after =
          styled st {mdLine = Fence, mdFence = Nothing} (ws <> line) afterLine
      | otherwise = styled st {mdLine = Code} ws rest
      where
        (run, after) = span (== c) line

    fenceOpen c
      | run == rest && not eof = Nothing
      | length run >= 3 = styled st {mdLine = Fence, mdFence = Just run} (ws <> line) afterLine
      | otherwise = emit ws rest st
      where
        run = takeWhile (== c) rest

    heading = case span (== '#') rest of
      (run, r)
        | null r && not eof -> Nothing
        | length run <= 6, take 1 r `elem` ["", " ", "\n"] ->
            styled st {mdLine = Heading} "" (if take 1 r == " " then drop 1 r else r)
        | otherwise -> emit ws rest st

    -- A thematic break such as @***@ prints as typed, not as emphasis.
    rule c
      | not lineDone = Nothing
      | length (filter (== c) line) >= 3 = emit (ws <> line) afterLine st
      | c == '*' = bullet c
      | otherwise = emit ws rest st

    bullet c = case drop 1 rest of
      [] | not eof -> Nothing
      n : _ | n == ' ' -> emit (ws <> [c, ' ']) (drop 2 rest) st {mdPrev = ' '}
      _ -> emit ws rest st

inline :: Bool -> Markdown -> String -> Maybe (Text, Markdown, String)
inline eof st s = case s of
  [] -> Just ("", st, [])
  '\n' : r -> Just (reset st <> "\n", markdown {mdFence = mdFence st}, r)
  _ | mdLine st `elem` [Fence, Code] -> literal (break (== '\n') s)
  '`' : _ ->
    let (run, r) = span (== '`') s
        n = length run
     in if null r && not eof
          then Nothing
          else case mdSpan st of
            0 -> change st {mdSpan = n} r
            m | m == n -> change st {mdSpan = 0} r
            _ -> literal (run, r)
  _ | mdSpan st > 0 -> literal (break (`elem` ['`', '\n']) s)
  '\\' : r -> case r of
    [] | not eof -> Nothing
    c : r' | isPunctuation c || isSymbol c -> literal ([c], r')
    _ -> literal ("\\", r)
  c : _ | c `elem` ['*', '_'] ->
    let (run, r) = span (== c) s
     in if null r && not eof then Nothing else emphasis c run r
  _ -> literal (break (`elem` ['\n', '`', '\\', '*', '_']) s)
  where
    literal (out, r) = Just (T.pack out, st {mdPrev = if null out then mdPrev st else last out}, r)
    change st' r = Just (restyle st st', st' {mdPrev = '`'}, r)

    -- CommonMark's flanking rules, simplified: an opener is followed by
    -- non-space, a closer preceded by non-space, and @_@ never acts
    -- inside a word, so snake_case stays literal.
    emphasis c run r =
      case (length run, mdStrong st, mdEm st) of
        (1, _, True) | canClose -> toggle st {mdEm = False}
        (1, _, False) | canOpen -> toggle st {mdEm = True}
        (2, True, _) | canClose -> toggle st {mdStrong = False}
        (2, False, _) | canOpen -> toggle st {mdStrong = True}
        (3, True, True) | canClose -> toggle st {mdStrong = False, mdEm = False}
        (3, False, False) | canOpen -> toggle st {mdStrong = True, mdEm = True}
        _ -> literal (run, r)
      where
        next = case r of
          n : _ -> n
          [] -> ' '
        prev = mdPrev st
        canOpen = not (isSpace next) && (c == '*' || not (isAlphaNum prev))
        canClose = not (isSpace prev) && (c == '*' || not (isAlphaNum next))
        toggle st' = Just (restyle st st', st' {mdPrev = c}, r)

-- | Escape codes that switch from the style of @old@ to that of @new@.
restyle :: Markdown -> Markdown -> Text
restyle old new
  | codes old == codes new = ""
  | otherwise = "\ESC[0m" <> sgr (codes new)
  where
    sgr [] = ""
    sgr cs = "\ESC[" <> T.intercalate ";" cs <> "m"

-- | Clear any active style.
reset :: Markdown -> Text
reset st = if null (codes st) then "" else "\ESC[0m"

codes :: Markdown -> [Text]
codes st = concat
  [ ["1" | mdStrong st || mdLine st == Heading]
  , ["2" | mdLine st == Fence]
  , ["3" | mdEm st]
  , ["35" | mdLine st == Heading]
  , ["36" | mdSpan st > 0]
  ]
