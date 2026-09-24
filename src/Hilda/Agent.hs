-- | The agent loop: call the model, run the tool calls it asks for, feed
-- the results back, repeat until it answers without tool calls.
--
-- The loop is polymorphic in its monad. Headless runs use 'IO'; the REPL
-- runs in haskeline's 'System.Console.Haskeline.InputT' so confirmation prompts share its terminal.
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

-- | What the loop reports to its caller while a turn runs.
data Event
  = Narration Text -- ^ Text the model sent alongside tool calls.
  | CallStarted ToolCall -- ^ After authorisation, before the tool runs.
  | CallFinished ToolCall (Either Text Text) -- ^ The tool's output or error.
  | ContextTrimmed Int Int -- ^ Tool results elided, characters removed.
  | CostUnknown -- ^ A cost limit is set but the provider reports no cost.
  | ReasoningDropped -- ^ The provider rejected kept reasoning; resent without it.

-- | How the loop reports events and asks the user.
data Hooks m = Hooks
  { onEvent :: Event -> m ()
  , confirm :: ToolCall -> m Bool -- ^ Asked on a 'Confirm' verdict; True runs the call.
  }

-- | Everything one turn needs: backend, model, tools, limits and hooks.
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

-- | Why a turn ended.
data Stop = Finished | TurnLimit | CostLimit | Failed Text
  deriving stock (Eq, Show)

-- | The result of 'runTurn'.
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
            Left err
              -- Dropped from the whole history: after a reroute it holds
              -- signatures from two upstreams, and neither accepts both.
              | rejectsReasoning err && any keepsReasoning hist -> do
                  onEvent (envHooks env) ReasoningDropped
                  go n usage ctx (map dropReasoning hist)
              | otherwise -> pure (Outcome hist "" n usage ctx (Failed err))
            Right (Reply text calls used reasoning) -> do
              when (n == 0 && isJust (envCostLimit env) && isNothing (usageCost used)) $
                onEvent (envHooks env) CostUnknown
              let hist' = hist <> [Assistant text calls reasoning]
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

-- | A provider refusing reasoning blocks it did not issue. OpenRouter can
-- route one Gemini conversation to Vertex and then AI Studio, and each
-- rejects the other's thought signatures.
rejectsReasoning :: Text -> Bool
rejectsReasoning err = any (`T.isInfixOf` T.toLower err) ["thought signature", "reasoning details"]

-- | An assistant message carrying reasoning_details blocks.
keepsReasoning :: Message -> Bool
keepsReasoning = \case
  Assistant _ _ (_ : _) -> True
  _ -> False

-- | The message without its reasoning_details blocks.
dropReasoning :: Message -> Message
dropReasoning = \case
  Assistant t calls _ -> Assistant t calls []
  m -> m

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

-- | Decode the arguments and run the tool. Bad JSON and IO errors become 'Left'.
runTool :: Tool -> Text -> IO (Either Text Text)
runTool tool raw = case decodeArgs raw of
  Left err -> pure (Left ("invalid arguments: " <> err))
  Right args -> either (Left . T.pack . show) id <$> try @IOException (toolRun tool args)
  where
    decodeArgs t
      | T.null (T.strip t) = Right (Object mempty)
      | otherwise = first T.pack (eitherDecodeStrict (encodeUtf8 t))
