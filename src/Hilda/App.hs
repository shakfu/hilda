-- | Resolved configuration and the headless runner.
module Hilda.App
  ( Config (..)
  , Output (..)
  , runHeadless
  , eventJson
  , outcomeJson
  , exitCodeFor
  , renderEvent
  , confirmQuestion
  , isYes
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hilda.Agent
import Hilda.Policy
import Hilda.Provider (ProviderKind, kindName)
import Hilda.Tools (builtinTools, truncateMiddle)
import Hilda.Types
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

data Output
  = Text       -- ^ Answer on stdout, tool activity on stderr.
  | Json       -- ^ One outcome object on stdout.
  | StreamJson -- ^ One object per event on stdout, then the outcome.
  deriving stock (Eq, Show)

-- | Run one prompt to completion and return the exit code.
runHeadless :: Config -> Output -> Text -> IO ExitCode
runHeadless cfg output prompt = do
  out <- runTurn env [System (cfgSystem cfg)] prompt
  case output of
    Text -> case outStop out of
      Finished  -> TIO.putStrLn (outText out)
      TurnLimit -> hPutStrLn stderr ("hilda: stopped after " <> show (outTurns out) <> " model calls (--max-turns)")
      Failed e  -> TIO.hPutStrLn stderr ("hilda: " <> e)
    _ -> jsonLine (outcomeJson cfg out)
  pure (exitCodeFor (outStop out))
  where
    env =
      Env
        { envComplete = cfgComplete cfg
        , envModel = cfgModel cfg
        , envTools = builtinTools
        , envMode = cfgMode cfg
        , envMaxTurns = cfgMaxTurns cfg
        , envHooks = Hooks {onEvent = emit, confirm = confirmTty}
        }
    emit = case output of
      Text       -> TIO.hPutStrLn stderr . renderEvent
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

confirmQuestion :: ToolCall -> Text
confirmQuestion call = "allow " <> callName call <> " " <> summarize (callArgs call) <> "? [y/N] "

isYes :: Text -> Bool
isYes t = T.toLower (T.strip t) `elem` ["y", "yes"]

renderEvent :: Event -> Text
renderEvent = \case
  Narration t -> t
  CallStarted c -> "> " <> callName c <> " " <> summarize (callArgs c)
  CallFinished _ (Left e) -> "  error: " <> summarize e
  CallFinished _ (Right r) -> "  ok (" <> T.pack (show (length (T.lines r))) <> " lines)"

-- | One line, at most 160 characters.
summarize :: Text -> Text
summarize t
  | T.length flat > 160 = T.take 157 flat <> "..."
  | otherwise = flat
  where
    flat = T.unwords (T.words t)

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
