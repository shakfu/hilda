-- | Pure decoding of streamed chat completions: server-sent-event framing
-- and folding delta chunks into a 'Reply'.
module Hilda.Stream
  ( Partial
  , emptyPartial
  , sseData
  , stepChunk
  , finishPartial
  , providerError
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
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
  }

data PartialCall = PartialCall
  { pcId   :: Text
  , pcName :: Text
  , pcArgs :: [Text]
  }

emptyPartial :: Partial
emptyPartial = Partial [] IM.empty mempty

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
stepChunk p = first T.pack . parseEither (withObject "chunk" chunk)
  where
    chunk o =
      o .:? "error" >>= \case
        Just e -> fail ("provider error: " <> providerError e)
        Nothing -> do
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
                }
            , [TextDelta t | Just t <- [text]] <> [ReasoningDelta t | Just t <- [thought], not (T.null t)]
            )

data CallDelta = CallDelta (Maybe Int) (Maybe Text) (Maybe Text) (Maybe Text)

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

finishPartial :: Partial -> Reply
finishPartial p = Reply text calls (partUsage p)
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

-- | The message of an error object, else its JSON.
providerError :: Value -> String
providerError = \case
  Object e | Just (String s) <- KM.lookup "message" e -> T.unpack s
  v -> TL.unpack (encodeToLazyText v)
