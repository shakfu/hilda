-- | Embed hilda's agent loop in another program.
--
-- One turn against OpenRouter, with the four built-in tools, confirming
-- every mutating call:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Hilda
-- > import qualified Data.Text.IO as TIO
-- >
-- > main :: IO ()
-- > main = do
-- >   let provider = Provider OpenRouter (defaultBaseUrl OpenRouter) (Just "sk-...") Nothing
-- >       budget = contextBudget Nothing
-- >   Right complete <- newComplete provider
-- >   let env = Env
-- >         { envComplete = withoutReasoning complete
-- >         , envModel = "anthropic/claude-sonnet-4.5"
-- >         , envTools = builtinTools (resultLimitFor budget)
-- >         , envMode = Ask
-- >         , envMaxTurns = 50
-- >         , envBudget = budget
-- >         , envCostLimit = Nothing
-- >         , envSpent = 0
-- >         , envHooks = Hooks {onEvent = const (pure ()), confirm = const (pure False)}
-- >         }
-- >   out <- runTurn env [System defaultSystemPrompt] "Summarise README.md"
-- >   TIO.putStrLn (outText out)
--
-- Pass 'outHistory' to the next 'runTurn' to continue the conversation.
-- The modules under @Hilda.*@ export more, for finer control.
module Hilda
  ( -- * Conversation
    Message (..)
  , ToolCall (..)
  , Usage (..)
  , Reply (..)
  , Finish (..)
  , Request (..)
  , Delta (..)
  , Complete

    -- * Backends
  , ProviderKind (..)
  , Provider (..)
  , kindName
  , parseKind
  , defaultBaseUrl
  , keyVariable
  , efforts
  , newComplete
  , withoutReasoning
  , lookupContext
  , tally

    -- * The agent loop
  , Env (..)
  , Hooks (..)
  , Event (..)
  , Stop (..)
  , Outcome (..)
  , runTurn

    -- * Tools and permissions
  , Tool (..)
  , Effect (..)
  , builtinTools
  , resultLimitFor
  , schema
  , withArgs
  , Mode (..)
  , Verdict (..)
  , modeName
  , parseMode
  , authorize

    -- * Context budget
  , contextBudget

    -- * System prompt
  , defaultSystemPrompt
  , assemble
  , discoverAgents
  , loadAgents

    -- * Sub-agents
  , Family
  , newFamily
  , familyComplete
  , attach
  , familyLimit
  , familySpent
  , familyUsage
  ) where

import Hilda.Agent
import Hilda.Context
import Hilda.Family
import Hilda.Policy
import Hilda.Prompt
import Hilda.Provider
import Hilda.Tools
import Hilda.Types
