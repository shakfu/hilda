-- | Chat-completions client for OpenAI-compatible servers and OpenRouter.
--
-- OpenRouter speaks the same protocol; it differs only in base URL, key
-- variable and attribution header, so both kinds share one client.
module Hilda.Provider
  ( ProviderKind (..)
  , Provider (..)
  , kindName
  , parseKind
  , defaultBaseUrl
  , keyVariable
  , efforts
  , encodeRequest
  , decodeReply
  , newComplete
  , newCompleteWith
  , withoutReasoning
  , endpointsContext
  , lookupContext
  , retryableStatus
  , retryableError
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, handle, try)
import Control.Monad (join)
import System.Timeout (timeout)
import Data.Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (dropWhileEnd)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Text.Read (readMaybe)
import Hilda.Stream
import Hilda.Types
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (newTlsManager, newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (Header, hContentType, statusCode)

-- | Backend variant. It selects the default base URL, key variable and headers.
data ProviderKind = OpenAICompatible | OpenRouter
  deriving stock (Eq, Show, Enum, Bounded)

-- | A resolved backend: kind, base URL and key.
data Provider = Provider
  { providerKind    :: ProviderKind
  , providerBaseUrl :: String
  , providerKey     :: Maybe Text -- ^ Absent for local servers that need none.
  , providerEffort  :: Maybe Text -- ^ Reasoning effort; one of 'efforts'.
  }
  deriving stock (Eq, Show)

-- | The name used by @--provider@ and in @models.json@.
kindName :: ProviderKind -> Text
kindName OpenAICompatible = "openai"
kindName OpenRouter       = "openrouter"

-- | Inverse of 'kindName', ignoring case.
parseKind :: Text -> Maybe ProviderKind
parseKind t = lookup (T.toLower t) [(kindName k, k) | k <- [minBound .. maxBound]]

-- | Base URL when no override is given.
defaultBaseUrl :: ProviderKind -> String
defaultBaseUrl OpenAICompatible = "https://api.openai.com/v1"
defaultBaseUrl OpenRouter       = "https://openrouter.ai/api/v1"

-- | Environment variable holding the API key, unless @--api-key-env@ names another.
keyVariable :: ProviderKind -> String
keyVariable OpenAICompatible = "OPENAI_API_KEY"
keyVariable OpenRouter       = "OPENROUTER_API_KEY"

-- | Reasoning efforts OpenRouter accepts. Each model supports a subset;
-- an unsupported one fails at the provider.
efforts :: [Text]
efforts = ["none", "minimal", "low", "medium", "high", "xhigh", "max"]

-- | The request body, with a reasoning effort if given.
--
-- Anthropic models need an explicit cache marker; other providers cache
-- automatically. OpenRouter moves a top-level marker to the last cacheable
-- block, so one field covers the whole growing conversation.
encodeRequest :: ProviderKind -> Maybe Text -> Request -> Value
encodeRequest kind effort r =
  object $
    [ "model" .= reqModel r
    , "messages" .= reqMessages r
    , "stream" .= True
    , "stream_options" .= object ["include_usage" .= True]
    ]
      <> ["cache_control" .= object ["type" .= ("ephemeral" :: Text)] | caches]
      <> maybe [] reasoning effort
      <> if null (reqTools r) then [] else ["tools" .= reqTools r, "tool_choice" .= ("auto" :: Text)]
  where
    caches = kind == OpenRouter && "anthropic/" `T.isPrefixOf` reqModel r
    reasoning e = case kind of
      OpenRouter       -> ["reasoning" .= object ["effort" .= e]]
      OpenAICompatible -> ["reasoning_effort" .= e]

-- | Decode a chat-completions response body. OpenRouter can report errors
-- with status 200 and an @error@ object, so that is checked first.
decodeReply :: Value -> Either Text Reply
decodeReply v = maybe (first T.pack (parseEither parser v)) Left (providerError v)
  where
    parser = withObject "response" $ \o ->
      o .: "choices" >>= \case
        [] -> fail "response has no choices"
        (c : _) -> do
          m <- c .: "message"
          Reply
            <$> (nonEmpty <$> m .:? "content")
            <*> m .:? "tool_calls" .!= []
            <*> o .:? "usage" .!= mempty
            <*> m .:? "reasoning_details" .!= []
    nonEmpty = \case
      Just t | not (T.null (T.strip t)) -> Just t
      _ -> Nothing

-- | Request headers. OpenRouter also gets @X-Title@ for attribution.
headers :: Provider -> [Header]
headers p =
  [("Content-Type", "application/json")]
    <> maybe [] (\k -> [("Authorization", "Bearer " <> encodeUtf8 k)]) (providerKey p)
    <> case providerKind p of
      OpenRouter       -> [("X-Title", "hilda")]
      OpenAICompatible -> []

-- | Drop reasoning_details from replies, so they are neither kept in the
-- history nor sent back.
withoutReasoning :: Complete -> Complete
withoutReasoning complete sink r = fmap (\rep -> rep {replyReasoning = []}) <$> complete sink r

-- | The smallest context window among a model's OpenRouter endpoints, in
-- tokens. The upstream is picked per request, so the smallest must fit.
endpointsContext :: Value -> Maybe Int
endpointsContext = parseMaybe $ withObject "response" $ \o -> do
  d <- o .: "data"
  eps <- d .: "endpoints"
  ns <- catMaybes <$> traverse (withObject "endpoint" (.:? "context_length")) eps
  if null ns then fail "no context length" else pure (minimum ns)

-- | A model's context window from OpenRouter's endpoints route, or Nothing
-- on any failure or after two seconds. The route is public, so no key is sent.
lookupContext :: Provider -> Text -> IO (Maybe Int)
lookupContext p model =
  case H.parseRequest (dropWhileEnd (== '/') (providerBaseUrl p) <> "/models/" <> T.unpack model <> "/endpoints") of
    Nothing -> pure Nothing
    Just req -> do
      mgr <- newTlsManager
      r <-
        timeout 2000000 . handle @IOException (const (pure Nothing)) $
          either (const Nothing) Just <$> try @H.HttpException (H.httpLbs req mgr)
      pure $ case join r of
        Just resp | statusCode (H.responseStatus resp) == 200 -> decode (H.responseBody resp) >>= endpointsContext
        _ -> Nothing

-- | Build a 'Complete' that shares one connection manager across calls.
-- Fails when the base URL does not parse.
newComplete :: Provider -> IO (Either Text Complete)
newComplete = newCompleteWith idleLimit

-- | 'newComplete' with the given idle limit in seconds.
newCompleteWith :: Int -> Provider -> IO (Either Text Complete)
newCompleteWith idle p =
  case H.parseRequest (dropWhileEnd (== '/') (providerBaseUrl p) <> "/chat/completions") of
    Nothing -> pure (Left ("invalid base URL: " <> T.pack (providerBaseUrl p)))
    Just base -> do
      mgr <- newTlsManagerWith tlsManagerSettings {H.managerResponseTimeout = H.responseTimeoutMicro (600 * 1000000)}
      let prepare r =
            base
              { H.method = "POST"
              , H.requestHeaders = headers p
              , H.requestBody = H.RequestBodyLBS (encode (encodeRequest (providerKind p) (providerEffort p) r))
              }
      pure (Right (\sink r -> send idle mgr (prepare r) sink))

-- | A retry carries the server's Retry-After, in seconds, if it sent one.
data Attempt = Retry (Maybe Int) Text | Done (Either Text Reply)

-- | POST with up to three retries, only where the server cannot have run
-- the request: 429, and connection failures before it was sent. A 5xx or
-- response timeout may follow a billed completion, so those fail at once.
-- A socket error mid-body reaches us as a raw 'IOException', not an
-- 'H.HttpException'; it fails at once for the same reason.
send :: Int -> H.Manager -> H.Request -> (Delta -> IO ()) -> IO (Either Text Reply)
send idle mgr req sink = go (0 :: Int)
  where
    retries = 3
    backoff n = threadDelay (1000000 * 2 ^ n)
    go n =
      try (handle @IOException lost (H.withResponse req mgr (receive idle sink))) >>= \case
        Left e
          | n < retries, retryableError e -> backoff n >> go (n + 1)
          | otherwise -> pure (Left (describe e))
        Right (Retry after err)
          | n < retries -> maybe (backoff n) (threadDelay . (* 1000000) . min 60) after >> go (n + 1)
          | otherwise -> pure (Left err)
        Right (Done r) -> pure r
    lost e = pure (Done (Left ("connection lost: " <> T.pack (show e))))
    -- Show only the failure, never the request: it carries the API key.
    describe = \case
      H.HttpExceptionRequest _ c -> "request failed: " <> T.pack (show c)
      H.InvalidUrlException u why -> "invalid URL " <> T.pack u <> ": " <> T.pack why

-- | Read one response. A server that ignores @stream@ answers with plain
-- JSON; its text reaches the sink in one piece.
receive :: Int -> (Delta -> IO ()) -> H.Response H.BodyReader -> IO Attempt
receive idle sink resp
  | retryableStatus code = Retry retryAfter . either id httpError <$> consume
  | code < 200 || code >= 300 = Done . Left . either id httpError <$> consume
  | streaming = Done <$> readStream idle (H.responseBody resp) sink
  | otherwise = do
      r <- (>>= \b -> first T.pack (eitherDecodeStrict b) >>= decodeReply) <$> consume
      either (const (pure ())) (mapM_ (sink . TextDelta) . replyText) r
      pure (Done r)
  where
    code = statusCode (H.responseStatus resp)
    consume = readBody idle (H.responseBody resp)
    httpError body = "HTTP " <> T.pack (show code) <> ": " <> decodeUtf8Lenient body
    -- Only the seconds form; an HTTP date falls back to the backoff.
    retryAfter = lookup "Retry-After" (H.responseHeaders resp) >>= readMaybe . BS8.unpack
    streaming = maybe False ("text/event-stream" `BS.isPrefixOf`) (lookup hContentType (H.responseHeaders resp))

-- | Seconds without any bytes before a response body counts as stalled.
-- The manager's response timeout covers only the status and headers.
idleLimit :: Int
idleLimit = 300

-- | One chunk, or Nothing after @idle@ seconds without data.
readIdle :: Int -> H.BodyReader -> IO (Maybe BS.ByteString)
readIdle idle body = timeout (idle * 1000000) (H.brRead body)

-- | Error text for a body idle for @idle@ seconds.
stalled :: Int -> Text
stalled idle = "response stalled: no data for " <> T.pack (show idle) <> "s"

-- | The whole body, failing if it stalls.
readBody :: Int -> H.BodyReader -> IO (Either Text BS.ByteString)
readBody idle body = go []
  where
    go acc =
      readIdle idle body >>= \case
        Nothing -> pure (Left (stalled idle))
        Just chunk
          | BS.null chunk -> pure (Right (BS.concat (reverse acc)))
          | otherwise -> go (chunk : acc)

-- | Fold server-sent events into a reply, passing text deltas to the sink.
readStream :: Int -> H.BodyReader -> (Delta -> IO ()) -> IO (Either Text Reply)
readStream idle body sink = loop BS.empty emptyPartial
  where
    loop buf p =
      readIdle idle body >>= \case
        Nothing -> pure (Left (stalled idle))
        Just chunk
          -- End of body without [DONE]: flush the last line and finish.
          | BS.null chunk -> feed (fst (sseData (buf <> "\n"))) Nothing p
          | otherwise -> let (payloads, rest) = sseData (buf <> chunk) in feed payloads (Just rest) p
    feed [] rest p = maybe (pure (Right (finishPartial p))) (`loop` p) rest
    -- Read to the end after [DONE] so the connection can be reused.
    feed ("[DONE]" : _) _ p = Right (finishPartial p) <$ timeout 5000000 (H.brConsume body)
    feed (d : ds) rest p =
      case first T.pack (eitherDecodeStrict d) >>= stepChunk p of
        Left e -> pure (Left e)
        Right (p', deltas) -> mapM_ sink deltas >> feed ds rest p'

-- | Statuses worth retrying: only 429, since the server did not run the request.
retryableStatus :: Int -> Bool
retryableStatus = (== 429)

-- | Connection failures before the request was sent, so nothing was billed.
retryableError :: H.HttpException -> Bool
retryableError = \case
  H.HttpExceptionRequest _ (H.ConnectionFailure _) -> True
  H.HttpExceptionRequest _ H.ConnectionTimeout -> True
  _ -> False
