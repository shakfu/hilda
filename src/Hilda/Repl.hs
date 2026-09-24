-- | Interactive session. Each line is a prompt or a slash command; the
-- session state is a value threaded through the input loop.
module Hilda.Repl
  ( Command (..)
  , Input (..)
  , parseInput
  , runRepl
  , tally
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hilda.Agent
import Hilda.App
import Hilda.Policy
import Hilda.Render
import Hilda.Tools
import Hilda.Types
import System.Console.Haskeline
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (stdout)

-- | A slash command.
data Command
  = Help
  | Quit
  | Clear
  | SetMode (Maybe Text)
  | SetModel (Maybe Text)
  | ListTools
  | ShowSystem
  | ShowUsage
  | Unknown Text
  deriving stock (Eq, Show)

-- | One REPL line: a prompt for the model or a command.
data Input = Prompt Text | Run Command
  deriving stock (Eq, Show)

-- | A line starting with @/@ is a command; @//@ escapes a prompt that
-- starts with @/@.
parseInput :: Text -> Input
parseInput line
  | Just rest <- T.stripPrefix "//" line = Prompt ("/" <> rest)
  | otherwise = case T.words line of
      (w : rest) | Just name <- T.stripPrefix "/" w -> Run (command name (listToMaybe rest))
      _ -> Prompt line
  where
    command name arg = case name of
      "help"   -> Help
      "quit"   -> Quit
      "exit"   -> Quit
      "clear"  -> Clear
      "mode"   -> SetMode arg
      "model"  -> SetModel arg
      "tools"  -> ListTools
      "system" -> ShowSystem
      "usage"  -> ShowUsage
      _        -> Unknown name

-- | REPL state carried between lines. Spending lives in the 'tally' ref.
data Session = Session
  { sesMode    :: Mode
  , sesModel   :: Text
  , sesHistory :: [Message]
  , sesContext :: Int -- ^ Prompt tokens of the last model call.
  , sesCalls   :: Int -- ^ Model calls in finished turns, across /clear.
  }

-- | Add the usage of each completed call to @ref@, so calls in a turn
-- abandoned with Ctrl-C still count toward the session.
tally :: IORef Usage -> Complete -> Complete
tally ref complete sink req = do
  r <- complete sink req
  mapM_ (modifyIORef' ref . (<>) . replyUsage) r
  pure r

-- | Streams reply text to stdout, with a waiting line before it on a
-- terminal.
runRepl :: Config -> IO ()
runRepl cfg0 = do
  terminal <- ansiTerminal stdout
  paint <- (\on -> if on then ansi else plain) <$> colorEnabled stdout
  spent <- newIORef mempty
  interactive paint spent cfg0 {cfgComplete = tally spent (liveOutput stdout paint (Live terminal True) (cfgComplete cfg0))}

-- | @spent@ holds the session's usage; the backend in @cfg@ adds to it.
interactive :: Paint -> IORef Usage -> Config -> IO ()
interactive paint spent cfg = do
  dir <- getXdgDirectory XdgState "hilda"
  createDirectoryIfMissing True dir
  runInputT defaultSettings {historyFile = Just (dir </> "history")} $ do
    say (paint BoldMagenta versionText)
    say (paint Dim ("model " <> cfgModel cfg <> ", mode " <> modeName (cfgMode cfg) <> ". /help for commands, Ctrl-D to exit."))
    -- The prompt stays uncolored: escape codes in it break haskeline's
    -- cursor arithmetic.
    _ <- loop (Session (cfgMode cfg) (cfgModel cfg) fresh 0 0)
    total <- liftIO (readIORef spent)
    say (paint Dim ("session: " <> renderUsage total))
  where
    fresh = [System (cfgSystem cfg)]

    loop s =
      getInputLine "hilda> " >>= \case
        Nothing -> pure s
        Just raw -> case T.strip (T.pack raw) of
          "" -> loop s
          line -> case parseInput line of
            Run Quit    -> pure s
            Run cmd     -> run cmd s >>= loop
            Prompt text -> turn s text >>= loop

    -- Ctrl-C abandons the turn and keeps the history from before it.
    turn s line = handleInterrupt (say (paint Red "[interrupted]") >> pure s) . withInterrupt $ do
      before <- liftIO (readIORef spent)
      out <- runTurn (env s (fromMaybe 0 (usageCost before))) (sesHistory s) line
      -- Reply text was already streamed by 'liveOutput'.
      case outStop out of
        Finished  -> pure ()
        TurnLimit -> say (paint Red "[stopped: turn limit reached]")
        CostLimit -> say (paint Red "[stopped: cost limit reached]")
        Failed e  -> say (paint Red ("error: " <> e))
      total <- liftIO (readIORef spent)
      when (outTurns out > 0) (liftIO (cfgRemember cfg (sesModel s)))
      say (paint Dim ("[turn: " <> renderUsage (outUsage out) <> " | session: " <> renderUsage total <> " | context: " <> tshow (outContext out) <> "]"))
      pure s {sesHistory = outHistory out, sesContext = outContext out, sesCalls = sesCalls s + outTurns out}

    env s spentCost =
      Env
        { envComplete = cfgComplete cfg
        , envModel = sesModel s
        , envTools = builtinTools
        , envMode = sesMode s
        , envMaxTurns = cfgMaxTurns cfg
        , envBudget = cfgBudget cfg
        , envCostLimit = cfgCostLimit cfg
        , envSpent = spentCost
        , envHooks =
            Hooks
              { onEvent = \case
                  Narration _ -> pure ()
                  CostUnknown | sesCalls s > 0 -> pure ()
                  ev -> case renderEvent paint ev of
                    Partial t -> outputStr (T.unpack t)
                    Full t    -> say t
              , confirm = \c -> do
                  say (confirmDetail c)
                  maybe False (isYes . T.pack) <$> getInputLine (T.unpack (confirmQuestion c))
              }
        }

    run cmd s = case cmd of
      Help -> s <$ mapM_ say helpText
      -- Spending stays with the session, so /clear does not reset --max-cost.
      Clear -> s {sesHistory = fresh, sesContext = 0} <$ say "history cleared"
      SetMode Nothing -> s <$ say ("mode: " <> modeName (sesMode s))
      SetMode (Just m) -> case parseMode m of
        Just mode -> s {sesMode = mode} <$ say ("mode: " <> modeName mode)
        Nothing -> s <$ say (paint Red "modes: yolo, ask, read-only")
      SetModel Nothing -> s <$ say ("model: " <> sesModel s)
      SetModel (Just m) -> s {sesModel = m} <$ say ("model: " <> m)
      ListTools ->
        s <$ mapM_ (\t -> say (toolName t <> " - " <> toolDescription t)) (visibleTools (sesMode s) builtinTools)
      ShowSystem -> s <$ say (cfgSystem cfg)
      ShowUsage -> do
        total <- liftIO (readIORef spent)
        s <$ say ("session: " <> renderUsage total <> " | context: " <> tshow (sesContext s) <> " of " <> tshow (cfgBudget cfg) <> " budget")
      Unknown name -> s <$ say (paint Red ("unknown command /" <> name <> "; try /help"))
      Quit -> pure s

    say = outputStrLn . T.unpack
    tshow = T.pack . show

-- | Printed by @/help@.
helpText :: [Text]
helpText =
  [ "/mode [yolo|ask|read-only]  show or set the permission mode"
  , "/model [name]               show or set the model"
  , "/tools                      list tools available in this mode"
  , "/system                     print the system prompt"
  , "/usage                      tokens and cost for this session"
  , "/clear                      start a new conversation"
  , "/quit                       exit (or Ctrl-D)"
  , "//text                      send a prompt that starts with /"
  ]
