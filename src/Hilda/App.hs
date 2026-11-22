-- | Resolved configuration and the headless runner.
module Hilda.App
  ( Config (..)
  , Output (..)
  , Saved (..)
  , contextBudget
  , versionText
  , runHeadless
  , headlessFailure
  , eventJson
  , deltaJson
  , outcomeJson
  , exitCodeFor
  , familyReport
  ) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import Data.Version (showVersion)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hilda.Agent
import Hilda.Family
import Hilda.Policy
import Hilda.Markdown (renderMarkdown)
import Hilda.Render
import Hilda.Provider (ProviderKind, kindName)
import Hilda.Tools (builtinTools, resultLimitFor)
import Hilda.Types
import Paths_hilda (version)
import System.Exit (ExitCode (..))
import System.IO

-- | Settings resolved from options and environment, shared by headless runs and the REPL.
data Config = Config
  { cfgComplete :: Complete
  , cfgProvider :: ProviderKind
  , cfgModel    :: Text
  , cfgMode     :: Mode
  , cfgSystem   :: Text
  , cfgMaxTurns :: Int
  , cfgBudget   :: Text -> IO Int -- ^ Context budget for a model, in estimated tokens.
  , cfgWindow   :: Text -> IO (Maybe Int) -- ^ A model's context window in tokens, if known.
  , cfgCostLimit :: Maybe Double -- ^ Spending limit in credits (USD on OpenRouter).
  , cfgRemember :: Text -> IO () -- ^ Record a model that answered, for the next run.
  , cfgColor    :: Bool -- ^ Color status and tool lines on a terminal (off: @--no-color@).
  , cfgMarkdown :: When -- ^ When to style reply text as markdown (@--markdown@).
  , cfgFamily   :: Maybe Family -- ^ @-r@: the @agent@ tool and the family budget.
  }

-- | Shown by @--version@ and at REPL start.
versionText :: Text
versionText = "hilda agent " <> T.pack (showVersion version)

-- | Output format of a headless run.
data Output
  = Text       -- ^ Answer on stdout, tool activity on stderr.
  | Json       -- ^ One outcome object on stdout.
  | StreamJson -- ^ One object per event on stdout, then the outcome.
  deriving stock (Eq, Show)

-- | Budget in tokens for a context window: half of it, leaving room for the
-- reply and for undercounting at 4 characters per token, at most 100000,
-- which also limits the cost per call. 100000 when the window is unknown.
contextBudget :: Maybe Int -> Int
contextBudget = maybe cap (\n -> max 1 (min cap (n `div` 2)))
  where
    cap = 100000

-- | The session a headless run continues and saves.
data Saved = Saved
  { savedId      :: Text
  , savedHistory :: [Message] -- ^ Earlier messages, without the system prompt.
  , savedWrite   :: Maybe ([Message] -> IO ()) -- ^ Nothing: @--no-session@, no transcript is saved.
  }

-- | Run one prompt to completion after the saved history, save the new
-- history, and return the exit code.
runHeadless :: Config -> Output -> Saved -> Text -> IO ExitCode
runHeadless cfg output saved prompt = do
  budget <- cfgBudget cfg (cfgModel cfg)
  paint <- (\on -> if on && cfgColor cfg then ansi else plain) <$> colorEnabled stderr
  styled <- markdownEnabled (cfgMarkdown cfg) stdout
  tty <- hIsTerminalDevice stdout
  -- Escape model text bound for a terminal or a pager; piped plain text is data.
  let answer = (if styled then renderMarkdown else id) . (if styled || tty then printable else id)
  terminal <- ansiTerminal stderr
  let complete = case output of
        -- Text mode keeps stdout for the final answer, so it does not echo.
        Text | terminal -> liveOutput stderr paint (Live True False False) (cfgComplete cfg)
        StreamJson -> \sink -> cfgComplete cfg (\d -> jsonLine (deltaJson d) >> sink d)
        _ -> cfgComplete cfg
  out <- runTurn (attach (cfgFamily cfg) (cfgSystem cfg) (env budget complete (emit paint))) (System (cfgSystem cfg) : savedHistory saved) prompt
  family <- traverse familyReport (cfgFamily cfg)
  when (outTurns out > 0) (cfgRemember cfg (cfgModel cfg))
  -- An unsaved session reports no id, so a caller cannot resume a stale one.
  sid <- case savedWrite saved of
    Nothing -> pure Nothing
    Just write ->
      try @IOException (write (drop 1 (outHistory out))) >>= \case
        Left e -> Nothing <$ hPutStrLn stderr ("hilda: session not saved: " <> show e)
        Right () -> pure (Just (savedId saved))
  case output of
    Text -> do
      case outStop out of
        Finished  -> TIO.putStrLn (answer (outText out))
        TurnLimit -> hPutStrLn stderr ("hilda: stopped after " <> show (outTurns out) <> " model calls (--max-turns)")
        CostLimit -> TIO.hPutStrLn stderr ("hilda: stopped at the cost limit, " <> maybe "" formatCost (usageCost (outUsage out)) <> " spent (--max-cost)")
        Failed e  -> TIO.hPutStrLn stderr ("hilda: " <> printable e)
      TIO.hPutStrLn stderr (paint Dim ("[" <> renderUsage (outUsage out) <> " | context: " <> T.pack (show (outContext out)) <> maybe "" ((" | " <>) . fst) family <> maybe "" (" | session " <>) sid <> "]"))
    _ -> jsonLine (withField "family" (snd <$> family) (outcomeJson cfg sid out))
  pure (exitCodeFor (outStop out))
  where
    env budget complete onEv =
      Env
        { envComplete = complete
        , envModel = cfgModel cfg
        , envTools = builtinTools (resultLimitFor budget)
        , envMode = cfgMode cfg
        , envMaxTurns = cfgMaxTurns cfg
        , envBudget = budget
        , envCostLimit = cfgCostLimit cfg
        , envSpent = 0
        , envHooks = Hooks {onEvent = onEv, confirm = confirmTty}
        }
    emit paint = case output of
      Text -> \ev -> case renderEvent paint ev of
        Partial t -> TIO.hPutStr stderr t >> hFlush stderr
        Full t    -> TIO.hPutStrLn stderr t
      Json       -> const (pure ())
      StreamJson -> jsonLine . eventJson
    -- Flush per line: stdout is block-buffered when piped.
    jsonLine v = BL.putStrLn (encode v) >> hFlush stdout

-- | The family's spending as text and as JSON.
familyReport :: Family -> IO (Text, Value)
familyReport fam = do
  usage <- familyUsage fam
  spent <- familySpent fam
  let tshow = T.pack . show
  pure
    ( "family: " <> renderUsage usage <> ", " <> tshow spent <> " of " <> tshow (familyLimit fam) <> " budget tokens"
    , object ["usage" .= usage, "spent_tokens" .= spent, "budget_tokens" .= familyLimit fam]
    )

-- | Add a field to a JSON object when the value is present.
withField :: Key -> Maybe Value -> Value -> Value
withField k (Just v) (Object o) = Object (KM.insert k v o)
withField _ _ v = v

-- | Report an error that stopped a headless run before it began: on
-- stderr, and with @--json@ or @--stream-json@ also as the result object.
headlessFailure :: Config -> Output -> Text -> IO ExitCode
headlessFailure cfg output e = do
  TIO.hPutStrLn stderr ("hilda: " <> e)
  when (output /= Text) $
    BL.putStrLn (encode (outcomeJson cfg Nothing (Outcome [] "" 0 mempty 0 (Failed e) FinishStop))) >> hFlush stdout
  pure (exitCodeFor (Failed e))

-- | Ask on the terminal. Without one (piped stdin, CI), or at end of
-- input, the answer is no.
confirmTty :: ToolCall -> IO Bool
confirmTty call = do
  tty <- hIsTerminalDevice stdin
  if not tty
    then pure False
    else do
      TIO.hPutStrLn stderr (confirmDetail call)
      TIO.hPutStr stderr (confirmQuestion call)
      hFlush stderr
      either (const False) (isYes . T.pack) <$> try @IOException getLine

-- | Stream-json line for one piece of a streamed reply.
deltaJson :: Delta -> Value
deltaJson = \case
  TextDelta t -> object ["type" .= ("text_delta" :: Text), "text" .= t]
  ReasoningDelta t -> object ["type" .= ("reasoning_delta" :: Text), "text" .= t]

-- | Stream-json line for one event. Tool output is truncated as the model sees it.
eventJson :: Event -> Value
eventJson = \case
  -- The deepest label wins: each child tags its own events.
  Sub l ev -> case eventJson ev of
    Object o | not (KM.member "agent" o) -> Object (KM.insert "agent" (String l) o)
    v -> v
  Narration t -> object ["type" .= ("text" :: Text), "text" .= t]
  CallStarted c ->
    object ["type" .= ("tool_call" :: Text), "id" .= callId c, "name" .= callName c, "arguments" .= callArgs c]
  CallFinished c r ->
    object
      [ "type" .= ("tool_result" :: Text)
      , "id" .= callId c
      , "name" .= callName c
      , "ok" .= either (const False) (const True) r
      , "output" .= either id id r
      ]
  ContextTrimmed n chars ->
    object ["type" .= ("context_trimmed" :: Text), "results" .= n, "characters" .= chars]
  CostUnknown -> object ["type" .= ("cost_unknown" :: Text)]
  ReasoningDropped -> object ["type" .= ("reasoning_dropped" :: Text)]

-- | The final @result@ object of @--json@ and @--stream-json@.
outcomeJson :: Config -> Maybe Text -> Outcome -> Value
outcomeJson cfg sid out =
  object
    [ "type" .= ("result" :: Text)
    , "result" .= outText out
    , "stop" .= stopName (outStop out)
    , "error" .= case outStop out of
        Failed e -> Just e
        _        -> Nothing
    , "turns" .= outTurns out
    , "usage" .= outUsage out
    , "context_tokens" .= outContext out
    , "provider" .= kindName (cfgProvider cfg)
    , "model" .= cfgModel cfg
    , "mode" .= modeName (cfgMode cfg)
    , "session_id" .= sid
    , "messages" .= outHistory out
    ]
  where
    stopName :: Stop -> Text
    stopName = \case
      Finished  -> "finished"
      TurnLimit -> "turn_limit"
      CostLimit -> "cost_limit"
      Failed _  -> "error"

-- | Exit code: 0 finished, 1 error, 2 turn limit, 3 cost limit.
exitCodeFor :: Stop -> ExitCode
exitCodeFor = \case
  Finished  -> ExitSuccess
  Failed _  -> ExitFailure 1
  TurnLimit -> ExitFailure 2
  CostLimit -> ExitFailure 3
