{-# LANGUAGE MultiWayIf #-}

-- | Recursive agents (@-r@): the @agent@ tool and the family budget.
--
-- A child is 'runTurn' in a green thread of the same process, with a
-- fresh history. It inherits the parent's mode, model and limits, and
-- its confirmations go to the parent's hooks. Every model call in the
-- family reserves tokens against one budget first. Design and limits:
-- @docs/dev/recursion.md@.
module Hilda.Family
  ( Family
  , newFamily
  , familyComplete
  , familyLimit
  , familySpent
  , familyHeld
  , familyUsage
  , attach
  , agentToolName
  , reservePercent
  , floorTokens
  , ceilingTokens
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (onException)
import Data.Aeson (encode, (.:))
import qualified Data.ByteString.Lazy as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Hilda.Agent
import Hilda.Context (historyTokens)
import Hilda.Tools (Effect (..), Tool (..), schema, withArgs)
import Hilda.Types

-- | Shared state of one family: the budget, the usage, the label counter
-- and the lock that keeps output and confirmations whole.
data Family = Family
  { famBase     :: Complete -- ^ The budgeted backend, without display wrappers.
  , famLimit    :: Int -- ^ @--budget-tokens@.
  , famCost     :: Maybe Double -- ^ @--max-cost@, for the whole family.
  , famMaxDepth :: Int -- ^ @--max-depth@: agents at this depth get no @agent@ tool.
  , famSpent    :: TVar Int -- ^ Tokens of settled requests.
  , famHeld     :: TVar Int -- ^ Tokens reserved by requests in flight.
  , famUsage    :: TVar Usage
  , famNext     :: TVar Int -- ^ The last child label number used.
  , famLock     :: MVar () -- ^ Held while an event prints or the user is asked.
  }

-- | Percent of the budget kept back, as in minima.
reservePercent :: Int
reservePercent = 5

-- | The smallest output grant a request goes out with.
floorTokens :: Int
floorTokens = 4096

-- | The largest output grant. Model output limits are not looked up.
ceilingTokens :: Int
ceilingTokens = 32000

-- | A family with a token budget, an optional cost limit and a depth limit,
-- over a backend.
newFamily :: Int -> Maybe Double -> Int -> Complete -> IO Family
newFamily limit cost depth base = do
  fam <-
    Family (\_ _ -> pure (Left "unset")) limit cost depth
      <$> newTVarIO 0
      <*> newTVarIO 0
      <*> newTVarIO mempty
      <*> newTVarIO 0
      <*> newMVar ()
  pure fam {famBase = budgeted fam base}

-- | The budgeted backend. The root agent uses it too.
familyComplete :: Family -> Complete
familyComplete = famBase

-- | @--budget-tokens@.
familyLimit :: Family -> Int
familyLimit = famLimit

-- | Tokens of settled requests, including failed ones at their full hold.
familySpent :: Family -> IO Int
familySpent = readTVarIO . famSpent

-- | Tokens reserved by requests in flight.
familyHeld :: Family -> IO Int
familyHeld = readTVarIO . famHeld

-- | Usage of every model call in the family so far.
familyUsage :: Family -> IO Usage
familyUsage = readTVarIO . famUsage

-- | Reserve, send with the grant as the output cap, then settle.
--
-- A request that fails or is cancelled settles at its full hold, since
-- whether the provider billed it is unknown; a 4xx response settles at 0. A reply without usage settles at the
-- estimate, so local servers are not charged the whole grant.
budgeted :: Family -> Complete -> Complete
budgeted fam base sink req = do
  over <- costReached
  if over
    then pure (Left "the family reached its cost limit (--max-cost)")
    else
      atomically (reserve fam input) >>= \case
        Nothing ->
          pure (Left ("the family token budget is spent (--budget-tokens " <> tshow (famLimit fam) <> ")"))
        Just grant -> do
          let hold = input + grant
          reply <- base sink req {reqMaxTokens = Just grant} `onException` atomically (settle hold hold)
          atomically $ case reply of
            -- A 4xx is a refusal before generation, so nothing was spent.
            Left err | "HTTP 4" `T.isPrefixOf` err -> settle hold 0
            Left _ -> settle hold hold
            Right r -> do
              settle hold (used r)
              modifyTVar' (famUsage fam) (<> replyUsage r)
          pure reply
  where
    input = historyTokens (reqMessages req) + fromIntegral (BL.length (encode (reqTools req))) `div` 4
    used r = case replyUsage r of
      u | usagePrompt u + usageCompletion u > 0 -> usagePrompt u + usageCompletion u
      _ -> input + historyTokens [Assistant (replyText r) (replyCalls r) []]
    settle hold n = modifyTVar' (famHeld fam) (subtract hold) >> modifyTVar' (famSpent fam) (+ n)
    costReached = case famCost fam of
      Nothing -> pure False
      Just limit -> maybe False (>= limit) . usageCost <$> readTVarIO (famUsage fam)

-- | The output grant for a request of @input@ tokens, holding it; Nothing
-- when settled spending leaves too little. Retries while only other
-- requests' holds stand in the way.
reserve :: Family -> Int -> STM (Maybe Int)
reserve fam input = do
  spent <- readTVar (famSpent fam)
  held <- readTVar (famHeld fam)
  let line = famLimit fam - famLimit fam * reservePercent `div` 100
      room = line - spent - held - input
  if
    | spent + input + floorTokens > line -> pure Nothing
    | room < floorTokens -> retry
    | otherwise -> do
        let grant = min ceilingTokens room
        Just grant <$ modifyTVar' (famHeld fam) (+ (input + grant))

-- | The name of the tool 'attach' adds.
agentToolName :: Text
agentToolName = "agent"

-- | The root agent's env with the @agent@ tool added, its hooks behind the
-- family lock. @system@ is the system prompt children start from.
attach :: Maybe Family -> Text -> Env IO -> Env IO
attach Nothing _ env = env
attach (Just fam) system env = withAgent fam system 0 env {envHooks = locked}
  where
    hooks = envHooks env
    locked =
      Hooks
        { onEvent = withMVar (famLock fam) . const . onEvent hooks
        , confirm = withMVar (famLock fam) . const . confirm hooks
        }

-- | Add the @agent@ tool to an agent at @depth@, unless it is at the limit.
withAgent :: Family -> Text -> Int -> Env IO -> Env IO
withAgent fam system depth env
  | depth >= famMaxDepth fam = env
  | otherwise = let env' = env {envTools = envTools env <> [agentTool fam system depth env']} in env'

-- | Delegate a task to a child of @parent@, which runs at @depth + 1@.
agentTool :: Family -> Text -> Int -> Env IO -> Tool
agentTool fam system depth parent =
  Tool
    { toolName = agentToolName
    , toolDescription =
        "Delegate a self-contained task to a sub-agent with a fresh context and the same tools. "
          <> "It cannot see this conversation or ask the user, so state everything it needs. "
          <> "Calls in one reply run concurrently. Returns its final answer."
    , toolParams = schema [("task", "string", "The task, with all the context it needs")] ["task"]
    , toolEffect = Delegate
    , toolRun = withArgs (.: "task") (runChild fam system depth parent)
    }

-- | Run one child to completion. Its call ids are prefixed with its label
-- and reply number wherever the parent sees them, since every child
-- conversation reuses ids such as @call_0@.
runChild :: Family -> Text -> Int -> Env IO -> Text -> IO (Either Text Text)
runChild fam system depth parent task = do
  n <- atomically (stateTVar (famNext fam) (\k -> (k + 1, k + 1)))
  replies <- newIORef (0 :: Int)
  let label = "agent-" <> tshow n
      tag c = (\k -> c {callId = label <> "." <> tshow k <> "/" <> callId c}) <$> readIORef replies
      tagged = \case
        CallStarted c -> CallStarted <$> tag c
        CallFinished c r -> (`CallFinished` r) <$> tag c
        ev -> pure ev
      up = envHooks parent
      child =
        withAgent fam system (depth + 1) $
          Env
            { envComplete = \sink r -> famBase fam sink r <* modifyIORef' replies (+ 1)
            , envModel = envModel parent
            , envTools = filter ((/= agentToolName) . toolName) (envTools parent)
            , envMode = envMode parent
            , envMaxTurns = envMaxTurns parent
            , envBudget = envBudget parent
            , envCostLimit = Nothing -- the family backend checks --max-cost
            , envSpent = 0
            , envHooks =
                Hooks
                  { onEvent = \ev -> tagged ev >>= onEvent up . Sub label
                  , confirm = \c -> tag c >>= confirm up
                  }
            }
  out <- runTurn child [System (T.strip system <> "\n\n" <> note)] task
  pure $ case outStop out of
    Finished
      | T.null (T.strip (outText out)) -> Right "(no answer)"
      | otherwise -> Right (outText out)
    TurnLimit -> Left (label <> " stopped after " <> tshow (outTurns out) <> " model calls (--max-turns)")
    CostLimit -> Left (label <> " stopped at the cost limit")
    Failed e -> Left (label <> " failed: " <> e)
  where
    note =
      "You are a sub-agent. Your final reply goes to the agent that delegated the task, "
        <> "not to the user, and you cannot ask the user anything. "
        <> "Do the task, then reply with the result."

tshow :: Show a => a -> Text
tshow = T.pack . show
