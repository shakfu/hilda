-- | Resolved configuration and the headless runner.
module Hilda.App
  ( Config (..)
  , Output (..)
  , versionText
  , runHeadless
  , eventJson
  , outcomeJson
  , exitCodeFor
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import Data.Version (showVersion)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hilda.Agent
import Hilda.Policy
import Hilda.Render
import Hilda.Provider (ProviderKind, kindName)
import Hilda.Tools (builtinTools, truncateMiddle)
import Hilda.Types
import Paths_hilda (version)
import System.Exit (ExitCode (..))
import System.IO

data Config = Config
  { cfgComplete :: Complete
  , cfgProvider :: ProviderKind
  , cfgModel    :: Text
  , cfgMode     :: Mode
  , cfgSystem   :: Text
  , cfgMaxTurns :: Int
  , cfgRemember :: Text -> IO () -- ^ Record the model for the next run.
  }

-- | Shown by @--version@ and at REPL start.
versionText :: Text
versionText = "hilda agent " <> T.pack (showVersion version)

data Output
  = Text       -- ^ Answer on stdout, tool activity on stderr.
  | Json       -- ^ One outcome object on stdout.
  | StreamJson -- ^ One object per event on stdout, then the outcome.
  deriving stock (Eq, Show)

-- | Run one prompt to completion and return the exit code.
runHeadless :: Config -> Output -> Text -> IO ExitCode
runHeadless cfg output prompt = do
  paint <- (\on -> if on then ansi else plain) <$> colorEnabled stderr
  out <- runTurn (env (emit paint)) [System (cfgSystem cfg)] prompt
  case output of
    Text -> do
      case outStop out of
        Finished  -> TIO.putStrLn (outText out)
        TurnLimit -> hPutStrLn stderr ("hilda: stopped after " <> show (outTurns out) <> " model calls (--max-turns)")
        Failed e  -> TIO.hPutStrLn stderr ("hilda: " <> e)
      TIO.hPutStrLn stderr (paint Dim ("[" <> renderUsage (outUsage out) <> "]"))
    _ -> jsonLine (outcomeJson cfg out)
  pure (exitCodeFor (outStop out))
  where
    env onEv =
      Env
        { envComplete = cfgComplete cfg
        , envModel = cfgModel cfg
        , envTools = builtinTools
        , envMode = cfgMode cfg
        , envMaxTurns = cfgMaxTurns cfg
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

-- | Ask on the terminal. Without one (piped stdin, CI) the answer is no.
confirmTty :: ToolCall -> IO Bool
confirmTty call = do
  tty <- hIsTerminalDevice stdin
  if not tty
    then pure False
    else do
      TIO.hPutStr stderr (confirmQuestion call)
      hFlush stderr
      isYes . T.pack <$> getLine

-- | Stream-json line for one event. Tool output is truncated as the model sees it.
eventJson :: Event -> Value
eventJson = \case
  Narration t -> object ["type" .= ("text" :: Text), "text" .= t]
  CallStarted c ->
    object ["type" .= ("tool_call" :: Text), "id" .= callId c, "name" .= callName c, "arguments" .= callArgs c]
  CallFinished c r ->
    object
      [ "type" .= ("tool_result" :: Text)
      , "id" .= callId c
      , "name" .= callName c
      , "ok" .= either (const False) (const True) r
      , "output" .= truncateMiddle resultLimit (either id id r)
      ]

outcomeJson :: Config -> Outcome -> Value
outcomeJson cfg out =
  object
    [ "type" .= ("result" :: Text)
    , "result" .= outText out
    , "stop" .= stopName (outStop out)
    , "error" .= case outStop out of
        Failed e -> Just e
        _        -> Nothing
    , "turns" .= outTurns out
    , "usage" .= outUsage out
    , "provider" .= kindName (cfgProvider cfg)
    , "model" .= cfgModel cfg
    , "mode" .= modeName (cfgMode cfg)
    , "messages" .= outHistory out
    ]
  where
    stopName :: Stop -> Text
    stopName = \case
      Finished  -> "finished"
      TurnLimit -> "turn_limit"
      Failed _  -> "error"

exitCodeFor :: Stop -> ExitCode
exitCodeFor = \case
  Finished  -> ExitSuccess
  Failed _  -> ExitFailure 1
  TurnLimit -> ExitFailure 2
