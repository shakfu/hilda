-- | Conversation data shared by the provider, the agent loop and the CLI.
--
-- Messages serialise to the OpenAI chat-completions wire format, which
-- OpenRouter and most local servers also accept.
module Hilda.Types
  ( ToolCall (..)
  , Message (..)
  , Usage (..)
  , Reply (..)
  , Request (..)
  , Complete
  ) where

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

data ToolCall = ToolCall
  { callId   :: Text
  , callName :: Text
  , callArgs :: Text -- ^ JSON-encoded arguments, as the model sent them.
  }
  deriving stock (Eq, Show)

data Message
  = System Text
  | User Text
  | Assistant (Maybe Text) [ToolCall]
  | ToolResult Text Text -- ^ Call id, content.
  deriving stock (Eq, Show)

data Usage = Usage
  { usagePrompt     :: !Int
  , usageCompletion :: !Int
  , usageCached     :: !Int -- ^ Prompt tokens served from the provider's cache.
  , usageCost       :: !(Maybe Double) -- ^ Reported by OpenRouter, in credits.
  }
  deriving stock (Eq, Show)

-- | Costs add where reported; a reply without one leaves the total alone.
instance Semigroup Usage where
  Usage a b c d <> Usage e f g h = Usage (a + e) (b + f) (c + g) (getSum <$> (Sum <$> d) <> (Sum <$> h))

instance Monoid Usage where
  mempty = Usage 0 0 0 Nothing

-- | One model response.
data Reply = Reply
  { replyText  :: Maybe Text
  , replyCalls :: [ToolCall]
  , replyUsage :: Usage
  }
  deriving stock (Eq, Show)

data Request = Request
  { reqModel    :: Text
  , reqMessages :: [Message]
  , reqTools    :: [Value] -- ^ Tool specs in function-calling format.
  }
  deriving stock (Eq, Show)

-- | A chat backend. It passes each piece of reply text to the sink as it
-- arrives. The agent loop depends only on this function type, so tests
-- substitute a scripted one and output modes wrap it to observe the text.
type Complete = (Text -> IO ()) -> Request -> IO (Either Text Reply)

instance ToJSON ToolCall where
  toJSON c =
    object
      [ "id" .= callId c
      , "type" .= ("function" :: Text)
      , "function" .= object ["name" .= callName c, "arguments" .= callArgs c]
      ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \o -> do
    f <- o .: "function"
    args <- f .:? "arguments" .!= String "{}"
    ToolCall <$> o .: "id" <*> f .: "name" <*> pure (argText args)
    where
      -- Some servers send arguments as an object instead of a string.
      argText (String s) = s
      argText v          = TL.toStrict (encodeToLazyText v)

instance ToJSON Message where
  toJSON = \case
    System t -> object ["role" .= ("system" :: Text), "content" .= t]
    User t -> object ["role" .= ("user" :: Text), "content" .= t]
    Assistant t [] -> object ["role" .= ("assistant" :: Text), "content" .= fromMaybe "" t]
    Assistant t calls ->
      object ["role" .= ("assistant" :: Text), "content" .= t, "tool_calls" .= calls]
    ToolResult i t ->
      object ["role" .= ("tool" :: Text), "tool_call_id" .= i, "content" .= t]

instance ToJSON Usage where
  toJSON u =
    object
      [ "prompt_tokens" .= usagePrompt u
      , "completion_tokens" .= usageCompletion u
      , "cached_tokens" .= usageCached u
      , "cost" .= usageCost u
      ]

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \o ->
    Usage
      <$> o .:? "prompt_tokens" .!= 0
      <*> o .:? "completion_tokens" .!= 0
      <*> (maybe (pure 0) (withObject "details" (\d -> d .:? "cached_tokens" .!= 0)) =<< o .:? "prompt_tokens_details")
      <*> o .:? "cost"
