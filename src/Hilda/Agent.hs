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
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Bifunctor (first)
import Data.Foldable (find, traverse_)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Hilda.Context
import Hilda.Policy
import Hilda.Tools
import Hilda.Types

data Event
  = Narration Text -- ^ Text the model sent alongside tool calls.
  | CallStarted ToolCall
  | CallFinished ToolCall (Either Text Text)
  | ContextTrimmed Int Int -- ^ Tool results elided, characters removed.
  | CostUnknown -- ^ A cost limit is set but the provider reports no cost.

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
  , envBudget   :: Int -- ^ Context budget in estimated tokens.
  , envCostLimit :: Maybe Double -- ^ Stop before a model call once spending reaches this.
  , envSpent    :: Double -- ^ Cost already spent, e.g. earlier in a REPL session.
  , envHooks    :: Hooks m
  }

data Stop = Finished | TurnLimit | CostLimit | Failed Text
  deriving stock (Eq, Show)

data Outcome = Outcome
  { outHistory :: [Message] -- ^ Full history, including this turn.
  , outText    :: Text      -- ^ Final assistant text; empty unless 'Finished'.
  , outTurns   :: Int       -- ^ Model calls made.
  , outUsage   :: Usage
  , outContext :: Int       -- ^ Prompt tokens of the last model call.
  , outStop    :: Stop
  }

-- | Append a user prompt to the history and run until the model stops.
runTurn :: MonadIO m => Env m -> [Message] -> Text -> m Outcome
runTurn env history prompt = go 0 mempty 0 (history <> [User prompt])
  where
    specs = map toolSpec (visibleTools (envMode env) (envTools env))
    go n usage ctx unfitted
      | n >= envMaxTurns env = pure (Outcome unfitted "" n usage ctx TurnLimit)
      -- Checked before a call, never between tool calls and their results,
      -- so the history stays valid for the next prompt.
      | spentAll usage = pure (Outcome unfitted "" n usage ctx CostLimit)
      | otherwise = do
          let (hist, elided, chars) = fitContext (envBudget env) unfitted
          when (elided > 0) (onEvent (envHooks env) (ContextTrimmed elided chars))
          liftIO (envComplete env (const (pure ())) (Request (envModel env) hist specs)) >>= \case
            Left err -> pure (Outcome hist "" n usage ctx (Failed err))
            Right (Reply text calls used) -> do
              when (n == 0 && isJust (envCostLimit env) && isNothing (usageCost used)) $
                onEvent (envHooks env) CostUnknown
              let hist' = hist <> [Assistant text calls]
                  usage' = usage <> used
                  -- Providers that report no usage get the estimate.
                  ctx' = if usagePrompt used > 0 then usagePrompt used else historyTokens hist
              if null calls
                then pure (Outcome hist' (fromMaybe "" text) (n + 1) usage' ctx' Finished)
                else do
                  traverse_ (onEvent (envHooks env) . Narration) text
                  results <- traverse (dispatch env) calls
                  go (n + 1) usage' ctx' (hist' <> results)
    spentAll usage = case envCostLimit env of
      Just limit -> envSpent env + fromMaybe 0 (usageCost usage) >= limit
      Nothing -> False

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
