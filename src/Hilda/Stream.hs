-- | Pure decoding of streamed chat completions: server-sent-event framing
-- and folding delta chunks into a t'Reply'.
module Hilda.Stream
  ( Partial
  , emptyPartial
  , sseData
  , stepChunk
  , finishPartial
  , mergeDetails
  , providerError
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Hilda.Types

-- | A reply under construction. Text and argument pieces are kept in
-- reverse order and joined once at the end.
data Partial = Partial
  { partText  :: [Text]
  , partCalls :: IntMap PartialCall
  , partUsage :: Usage
  , partDetails :: [Value] -- ^ reasoning_details fragments, newest first.
  }

-- | A tool call under construction; argument pieces newest first.
data PartialCall = PartialCall
  { pcId   :: Text
  , pcName :: Text
  , pcArgs :: [Text]
  }

-- | The state before the first chunk.
emptyPartial :: Partial
emptyPartial = Partial [] IM.empty mempty []

-- | Payloads of the complete @data:@ lines in a buffer, and the incomplete
-- remainder. Comments (@:@), other fields and blank lines are skipped.
sseData :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
sseData buf = (payloads, rest)
  where
    (complete, rest) = BS8.breakEnd (== '\n') buf
    payloads =
      [ BS8.dropWhile (== ' ') (BS.drop 5 l)
      | l <- map (BS8.dropWhileEnd (== '\r')) (BS8.lines complete)
      , "data:" `BS.isPrefixOf` l
      ]

-- | Fold one chunk into the reply; also return its deltas.
stepChunk :: Partial -> Value -> Either Text (Partial, [Delta])
stepChunk p v = maybe (first T.pack (parseEither (withObject "chunk" chunk) v)) Left (providerError v)
  where
    chunk o = do
      usage <- o .:? "usage"
      choices <- o .:? "choices" .!= []
      delta <- case choices of
        (c : _) -> withObject "choice" (\c' -> c' .:? "delta" .!= KM.empty) c
        [] -> pure KM.empty
      content <- delta .:? "content"
      calls <- traverse callDelta =<< (delta .:? "tool_calls" .!= [])
      -- Models send reasoning as reasoning_details, a plain reasoning
      -- string, or both; prefer the details to avoid counting it twice.
      details <- delta .:? "reasoning_details" .!= []
      plain <- delta .:? "reasoning"
      let text = content >>= \t -> if T.null t then Nothing else Just t
          thought = case [t | Object d <- details, Just (String t) <- [KM.lookup "text" d]] of
            [] -> plain
            ts -> Just (T.concat ts)
      pure
        ( p
            { partText = maybe id (:) text (partText p)
            , partCalls = foldl' mergeCall (partCalls p) calls
            , partUsage = fromMaybe (partUsage p) usage
            , partDetails = reverse details <> partDetails p
            }
        , [TextDelta t | Just t <- [text]] <> [ReasoningDelta t | Just t <- [thought], not (T.null t)]
        )

-- | One streamed tool-call piece: index, id, name and argument text.
data CallDelta = CallDelta (Maybe Int) (Maybe Text) (Maybe Text) (Maybe Text)

-- | Parse a tool-call piece. Arguments sent as an object are re-encoded as text.
callDelta :: Value -> Parser CallDelta
callDelta = withObject "tool call delta" $ \o -> do
  f <- o .:? "function" .!= KM.empty
  CallDelta
    <$> o .:? "index"
    <*> o .:? "id"
    <*> f .:? "name"
    <*> (fmap argText <$> f .:? "arguments")
  where
    argText (String s) = s
    argText Null       = ""
    argText v          = TL.toStrict (encodeToLazyText v)

-- | Pieces of one call share an index. Without an index, a new id opens a
-- new call and a piece without an id extends the latest one.
mergeCall :: IntMap PartialCall -> CallDelta -> IntMap PartialCall
mergeCall m (CallDelta ix cid name args) = IM.alter (Just . update . fromMaybe (PartialCall "" "" [])) slot m
  where
    slot = case (ix, cid) of
      (Just i, _) -> i
      (Nothing, Just c) -> fromMaybe nextSlot (lookup c [(pcId v, k) | (k, v) <- IM.toList m])
      (Nothing, Nothing) -> maybe 0 fst (IM.lookupMax m)
    nextSlot = maybe 0 ((+ 1) . fst) (IM.lookupMax m)
    update pc =
      pc
        { pcId = firstNonEmpty (pcId pc) cid
        , pcName = firstNonEmpty (pcName pc) name
        , pcArgs = maybe id (:) args (pcArgs pc)
        }
    firstNonEmpty old new = if T.null old then fromMaybe "" new else old

-- | The completed reply. Calls without an id get @call_N@; empty arguments become @{}@.
finishPartial :: Partial -> Reply
finishPartial p = Reply text calls (partUsage p) (mergeDetails (reverse (partDetails p)))
  where
    joined = T.concat (reverse (partText p))
    text = if T.null (T.strip joined) then Nothing else Just joined
    calls =
      [ ToolCall
          (if T.null i then "call_" <> T.pack (show k) else i)
          n
          (let a = T.concat (reverse args) in if T.null (T.strip a) then "{}" else a)
      | (k, PartialCall i n args) <- IM.toAscList (partCalls p)
      ]

-- | Join streamed reasoning_details fragments into whole blocks, in
-- first-seen order. Fragments sharing an @index@ are one block: their
-- @text@, @summary@ and @data@ strings are concatenated; for other fields
-- the last non-null value wins, since an Anthropic signature arrives in a
-- final text-less fragment. Entries without an index pass through.
mergeDetails :: [Value] -> [Value]
mergeDetails frags = go IS.empty frags
  where
    go _ [] = []
    go seen (v : vs) = case indexOf v of
      Nothing -> v : go seen vs
      Just i
        | i `IS.member` seen -> go seen vs
        | otherwise -> combine (IM.findWithDefault [] i groups) : go (IS.insert i seen) vs
    groups = IM.fromListWith (flip (<>)) [(i, [o]) | v@(Object o) <- frags, Just i <- [indexOf v]]
    indexOf = \case
      Object o -> parseMaybe (.: "index") o
      _ -> Nothing
    combine os = Object (KM.mapWithKey (joined os) (foldl' (KM.unionWith keep) KM.empty os))
    keep old new = if new == Null then old else new
    joined os k v
      | k `elem` ["text", "summary", "data"], ss@(_ : _) <- [s | o <- os, Just (String s) <- [KM.lookup k o]] = String (T.concat ss)
      | otherwise = v

-- | The error in a response body, if it is an error object: the message,
-- else the error's JSON. OpenRouter's message can be a generic "Provider
-- returned error", with the cause in @metadata.raw@, so that is appended.
-- Checked before parsing, so the text carries no aeson path prefix.
providerError :: Value -> Maybe Text
providerError = \case
  Object o | Just e <- KM.lookup "error" o, e /= Null -> Just ("provider error: " <> describe e)
  _ -> Nothing
  where
    describe = \case
      Object e | Just (String s) <- KM.lookup "message" e -> s <> raw e
      v -> TL.toStrict (encodeToLazyText v)
    raw e = case KM.lookup "metadata" e of
      Just (Object m) | Just (String r) <- KM.lookup "raw" m -> ": " <> T.strip r
      _ -> ""
