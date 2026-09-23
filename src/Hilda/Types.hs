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
  }
  deriving stock (Eq, Show)

instance Semigroup Usage where
  Usage a b <> Usage c d = Usage (a + c) (b + d)

instance Monoid Usage where
  mempty = Usage 0 0

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

-- | A chat backend. The agent loop depends only on this function type,
-- so tests substitute a scripted one.
type Complete = Request -> IO (Either Text Reply)

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
    object ["prompt_tokens" .= usagePrompt u, "completion_tokens" .= usageCompletion u]

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \o ->
    Usage <$> o .:? "prompt_tokens" .!= 0 <*> o .:? "completion_tokens" .!= 0
