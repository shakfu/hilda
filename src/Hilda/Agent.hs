-- | The agent loop: call the model, run the tool calls it asks for, feed
-- the results back, repeat until it answers without tool calls.
--
-- The loop is polymorphic in its monad. Headless runs use 'IO'; the REPL
-- runs in haskeline's 'InputT' so confirmation prompts share its terminal.
module Hilda.Agent
  ( Event (..)
  , Hooks (..)
  , Env (..)
  , Stop (..)
  , Outcome (..)
  , runTurn
  , resultLimit
  ) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Bifunctor (first)
import Data.Foldable (find, traverse_)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Hilda.Policy
import Hilda.Tools
import Hilda.Types

data Event
  = Narration Text -- ^ Text the model sent alongside tool calls.
  | CallStarted ToolCall
  | CallFinished ToolCall (Either Text Text)

data Hooks m = Hooks
  { onEvent :: Event -> m ()
  , confirm :: ToolCall -> m Bool
  }

data Env m = Env
  { envComplete :: Complete
  , envModel    :: Text
  , envTools    :: [Tool]
  , envMode     :: Mode
  , envMaxTurns :: Int
  , envHooks    :: Hooks m
  }

data Stop = Finished | TurnLimit | Failed Text
  deriving stock (Eq, Show)

data Outcome = Outcome
  { outHistory :: [Message] -- ^ Full history, including this turn.
  , outText    :: Text      -- ^ Final assistant text; empty unless 'Finished'.
  , outTurns   :: Int       -- ^ Model calls made.
  , outUsage   :: Usage
  , outStop    :: Stop
  }

-- | Append a user prompt to the history and run until the model stops.
runTurn :: MonadIO m => Env m -> [Message] -> Text -> m Outcome
runTurn env history prompt = go 0 mempty (history <> [User prompt])
  where
    specs = map toolSpec (visibleTools (envMode env) (envTools env))
    go n usage hist
      | n >= envMaxTurns env = pure (Outcome hist "" n usage TurnLimit)
      | otherwise =
          liftIO (envComplete env (Request (envModel env) hist specs)) >>= \case
            Left err -> pure (Outcome hist "" n usage (Failed err))
            Right (Reply text calls used) -> do
              let hist' = hist <> [Assistant text calls]
                  usage' = usage <> used
              if null calls
                then pure (Outcome hist' (fromMaybe "" text) (n + 1) usage' Finished)
                else do
                  traverse_ (onEvent (envHooks env) . Narration) text
                  results <- traverse (dispatch env) calls
                  go (n + 1) usage' (hist' <> results)

-- | Authorise and run one tool call. Every failure becomes a tool result
-- the model can read, so the loop itself never fails on a tool.
-- Confirmation happens before 'CallStarted', so a prompt never splits the
-- started and finished output of one call.
dispatch :: MonadIO m => Env m -> ToolCall -> m Message
dispatch env call = do
  permitted <- case find ((== callName call) . toolName) (envTools env) of
    Nothing -> pure (Left ("unknown tool: " <> callName call))
    Just tool -> case authorize (envMode env) (toolEffect tool) of
      Allow -> pure (Right tool)
      Deny why -> pure (Left why)
      Confirm ->
        confirm hooks call <&> \case
          True -> Right tool
          False -> Left "the user declined this tool call"
  onEvent hooks (CallStarted call)
  result <- either (pure . Left) (\tool -> liftIO (runTool tool (callArgs call))) permitted
  onEvent hooks (CallFinished call result)
  pure (ToolResult (callId call) (truncateMiddle resultLimit (either ("error: " <>) id result)))
  where
    hooks = envHooks env

runTool :: Tool -> Text -> IO (Either Text Text)
runTool tool raw = case decodeArgs raw of
  Left err -> pure (Left ("invalid arguments: " <> err))
  Right args -> either (Left . T.pack . show) id <$> try @IOException (toolRun tool args)
  where
    decodeArgs t
      | T.null (T.strip t) = Right (Object mempty)
      | otherwise = first T.pack (eitherDecodeStrict (encodeUtf8 t))
