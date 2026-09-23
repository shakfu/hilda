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
  , encodeRequest
  , decodeReply
  , newComplete
  , retryableStatus
  , retryableError
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import System.Timeout (timeout)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Hilda.Stream
import Hilda.Types
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (Header, hContentType, statusCode)

data ProviderKind = OpenAICompatible | OpenRouter
  deriving stock (Eq, Show, Enum, Bounded)

data Provider = Provider
  { providerKind    :: ProviderKind
  , providerBaseUrl :: String
  , providerKey     :: Maybe Text -- ^ Absent for local servers that need none.
  }
  deriving stock (Eq, Show)

kindName :: ProviderKind -> Text
kindName OpenAICompatible = "openai"
kindName OpenRouter       = "openrouter"

parseKind :: Text -> Maybe ProviderKind
parseKind t = lookup (T.toLower t) [(kindName k, k) | k <- [minBound .. maxBound]]

defaultBaseUrl :: ProviderKind -> String
defaultBaseUrl OpenAICompatible = "https://api.openai.com/v1"
defaultBaseUrl OpenRouter       = "https://openrouter.ai/api/v1"

keyVariable :: ProviderKind -> String
keyVariable OpenAICompatible = "OPENAI_API_KEY"
keyVariable OpenRouter       = "OPENROUTER_API_KEY"

-- | Anthropic models need an explicit cache marker; other providers cache
-- automatically. OpenRouter moves a top-level marker to the last cacheable
-- block, so one field covers the whole growing conversation.
encodeRequest :: ProviderKind -> Request -> Value
encodeRequest kind r =
  object $
    [ "model" .= reqModel r
    , "messages" .= reqMessages r
    , "stream" .= True
    , "stream_options" .= object ["include_usage" .= True]
    ]
      <> ["cache_control" .= object ["type" .= ("ephemeral" :: Text)] | caches]
      <> if null (reqTools r) then [] else ["tools" .= reqTools r, "tool_choice" .= ("auto" :: Text)]
  where
    caches = kind == OpenRouter && "anthropic/" `T.isPrefixOf` reqModel r

-- | Decode a chat-completions response body. OpenRouter can report errors
-- with status 200 and an @error@ object, so that is checked first.
decodeReply :: Value -> Either Text Reply
decodeReply = first T.pack . parseEither parser
  where
    parser = withObject "response" $ \o ->
      o .:? "error" >>= \case
        Just err -> fail ("provider error: " <> providerError err)
        Nothing ->
          o .: "choices" >>= \case
            [] -> fail "response has no choices"
            (c : _) -> do
              m <- c .: "message"
              Reply
                <$> (nonEmpty <$> m .:? "content")
                <*> m .:? "tool_calls" .!= []
                <*> o .:? "usage" .!= mempty
    nonEmpty = \case
      Just t | not (T.null (T.strip t)) -> Just t
      _ -> Nothing

headers :: Provider -> [Header]
headers p =
  [("Content-Type", "application/json")]
    <> maybe [] (\k -> [("Authorization", "Bearer " <> encodeUtf8 k)]) (providerKey p)
    <> case providerKind p of
      OpenRouter       -> [("X-Title", "hilda")]
      OpenAICompatible -> []

-- | Build a 'Complete' that shares one connection manager across calls.
-- Fails when the base URL does not parse.
newComplete :: Provider -> IO (Either Text Complete)
newComplete p =
  case H.parseRequest (dropWhileEnd (== '/') (providerBaseUrl p) <> "/chat/completions") of
    Nothing -> pure (Left ("invalid base URL: " <> T.pack (providerBaseUrl p)))
    Just base -> do
      mgr <- newTlsManagerWith tlsManagerSettings {H.managerResponseTimeout = H.responseTimeoutMicro (600 * 1000000)}
      let prepare r =
            base
              { H.method = "POST"
              , H.requestHeaders = headers p
              , H.requestBody = H.RequestBodyLBS (encode (encodeRequest (providerKind p) r))
              }
      pure (Right (\sink r -> send mgr (prepare r) sink))

data Attempt = Retry Text | Done (Either Text Reply)

-- | POST with up to three retries, only where the server cannot have run
-- the request: 429, and connection failures before it was sent. A 5xx or
-- response timeout may follow a billed completion, so those fail at once.
send :: H.Manager -> H.Request -> (Text -> IO ()) -> IO (Either Text Reply)
send mgr req sink = go (0 :: Int)
  where
    retries = 3
    backoff n = threadDelay (1000000 * 2 ^ n)
    go n =
      try (H.withResponse req mgr (receive sink)) >>= \case
        Left e
          | n < retries, retryableError e -> backoff n >> go (n + 1)
          | otherwise -> pure (Left (describe e))
        Right (Retry err)
          | n < retries -> backoff n >> go (n + 1)
          | otherwise -> pure (Left err)
        Right (Done r) -> pure r
    -- Show only the failure, never the request: it carries the API key.
    describe = \case
      H.HttpExceptionRequest _ c -> "request failed: " <> T.pack (show c)
      H.InvalidUrlException u why -> "invalid URL " <> T.pack u <> ": " <> T.pack why

-- | Read one response. A server that ignores @stream@ answers with plain
-- JSON; its text reaches the sink in one piece.
receive :: (Text -> IO ()) -> H.Response H.BodyReader -> IO Attempt
receive sink resp
  | retryableStatus code = Retry . httpError <$> consume
  | code < 200 || code >= 300 = Done . Left . httpError <$> consume
  | streaming = Done <$> readStream (H.responseBody resp) sink
  | otherwise = do
      r <- (\b -> first T.pack (eitherDecodeStrict b) >>= decodeReply) <$> consume
      either (const (pure ())) (mapM_ sink . replyText) r
      pure (Done r)
  where
    code = statusCode (H.responseStatus resp)
    consume = BS.concat <$> H.brConsume (H.responseBody resp)
    httpError body = "HTTP " <> T.pack (show code) <> ": " <> decodeUtf8Lenient body
    streaming = maybe False ("text/event-stream" `BS.isPrefixOf`) (lookup hContentType (H.responseHeaders resp))

-- | Seconds without any bytes before a stream counts as stalled.
streamIdleLimit :: Int
streamIdleLimit = 300

-- | Fold server-sent events into a reply, passing text deltas to the sink.
readStream :: H.BodyReader -> (Text -> IO ()) -> IO (Either Text Reply)
readStream body sink = loop BS.empty emptyPartial
  where
    loop buf p =
      timeout (streamIdleLimit * 1000000) (H.brRead body) >>= \case
        Nothing -> pure (Left ("stream stalled: no data for " <> T.pack (show streamIdleLimit) <> "s"))
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
        Right (p', delta) -> mapM_ sink delta >> feed ds rest p'

retryableStatus :: Int -> Bool
retryableStatus = (== 429)

retryableError :: H.HttpException -> Bool
retryableError = \case
  H.HttpExceptionRequest _ (H.ConnectionFailure _) -> True
  H.HttpExceptionRequest _ H.ConnectionTimeout -> True
  _ -> False
