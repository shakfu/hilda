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
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Hilda.Types
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (Header, statusCode)

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

encodeRequest :: Request -> Value
encodeRequest r =
  object $
    ["model" .= reqModel r, "messages" .= reqMessages r]
      <> if null (reqTools r) then [] else ["tools" .= reqTools r, "tool_choice" .= ("auto" :: Text)]

-- | Decode a chat-completions response body. OpenRouter can report errors
-- with status 200 and an @error@ object, so that is checked first.
decodeReply :: Value -> Either Text Reply
decodeReply = first T.pack . parseEither parser
  where
    parser = withObject "response" $ \o ->
      o .:? "error" >>= \case
        Just err -> fail ("provider error: " <> errorMessage err)
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
    errorMessage = \case
      Object e | Just (String s) <- KM.lookup "message" e -> T.unpack s
      v -> show v

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
              , H.requestBody = H.RequestBodyLBS (encode (encodeRequest r))
              }
      pure (Right (send mgr . prepare))

-- | POST with up to three retries on transport errors, 429 and 5xx.
send :: H.Manager -> H.Request -> IO (Either Text Reply)
send mgr req = go (0 :: Int)
  where
    retries = 3
    backoff n = threadDelay (1000000 * 2 ^ n)
    go n =
      try (H.httpLbs req mgr) >>= \case
        Left e
          | n < retries, retryable e -> backoff n >> go (n + 1)
          | otherwise -> pure (Left (describe e))
        Right resp
          | code == 429 || code >= 500, n < retries -> backoff n >> go (n + 1)
          | code >= 200 && code < 300 ->
              pure (first T.pack (eitherDecode body) >>= decodeReply)
          | otherwise ->
              pure (Left ("HTTP " <> T.pack (show code) <> ": " <> decodeUtf8Lenient (BL.toStrict body)))
          where
            code = statusCode (H.responseStatus resp)
            body = H.responseBody resp
    retryable = \case
      H.HttpExceptionRequest {} -> True
      H.InvalidUrlException {} -> False
    -- Show only the failure, never the request: it carries the API key.
    describe = \case
      H.HttpExceptionRequest _ c -> "request failed: " <> T.pack (show c)
      H.InvalidUrlException u why -> "invalid URL " <> T.pack u <> ": " <> T.pack why
