-- | Interactive session. Each entry is a prompt or a slash command; the
-- session state is a value threaded through the input loop. Line editing
-- is isocline's: Shift+Enter or Ctrl+J inserts a newline.
module Hilda.Repl
  ( Command (..)
  , Input (..)
  , parseInput
  , runRepl
  , tally
  ) where

import Control.Exception (AsyncException (UserInterrupt), IOException, handle, throwIO, try)
import Control.Monad (void, when)
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
import qualified Data.Text.IO as TIO
import System.Console.Isocline (enableBraceInsertion, enableBraceMatching, enableColor, enableHighlight, enableHint, enableMultiline, historyRemoveLast, readlineMaybe, setHistory)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

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
  , sesBudget  :: Int -- ^ Context budget for 'sesModel'.
  }

-- | Add the usage of each completed call to @ref@, so calls in a turn
-- abandoned with Ctrl-C still count toward the session.
tally :: IORef Usage -> Complete -> Complete
tally ref complete sink req = do
  r <- complete sink req
  mapM_ (modifyIORef' ref . (<>) . replyUsage) r
  pure r

-- | Streams reply text to stdout, with a waiting line before it on a
-- terminal. Starts from @restored@ and passes the history to @save@ after
-- each turn and @/clear@.
runRepl :: Config -> [Message] -> ([Message] -> IO ()) -> IO ()
runRepl cfg0 restored save = do
  terminal <- ansiTerminal stdout
  color <- colorEnabled stdout
  let paint = if color then ansi else plain
  spent <- newIORef mempty
  dir <- getXdgDirectory XdgState "hilda"
  createDirectoryIfMissing True dir
  setupEditor color (dir </> "history")
  interactive paint spent restored save cfg0 {cfgComplete = tally spent (liveOutput stdout paint (Live terminal True) (cfgComplete cfg0))}

-- | Line editing with history in @path@ and multi-line entries. Brace
-- insertion, hints and highlighting are off: prompts are prose, not code.
setupEditor :: Bool -> FilePath -> IO ()
setupEditor color path = do
  setHistory path 1000
  mapM_ (\f -> f False) [enableBraceInsertion, enableBraceMatching, enableHint, enableHighlight]
  void (enableColor color)
  void (enableMultiline True)

-- | @spent@ holds the session's usage; the backend in @cfg@ adds to it.
interactive :: Paint -> IORef Usage -> [Message] -> ([Message] -> IO ()) -> Config -> IO ()
interactive paint spent restored save cfg = do
  say (paint BoldMagenta versionText)
  say (paint Dim ("model " <> cfgModel cfg <> ", mode " <> modeName (cfgMode cfg) <> ". /help for commands, Shift+Enter for a new line, Ctrl-D to exit."))
  when (not (null restored)) (say (paint Dim ("resumed " <> tshow (length restored) <> " messages")))
  budget <- cfgBudget cfg (cfgModel cfg)
  _ <- loop (Session (cfgMode cfg) (cfgModel cfg) (fresh <> restored) 0 0 budget)
  total <- readIORef spent
  say (paint Dim ("session: " <> renderUsage total))
  where
    fresh = [System (cfgSystem cfg)]
    tools s = builtinTools (resultLimitFor (sesBudget s))

    -- Ctrl-C at the prompt clears the entry and reads as "".
    loop s =
      readlineMaybe "hilda" >>= \case
        Nothing -> pure s
        Just raw -> case T.strip (T.pack raw) of
          "" -> loop s
          line -> case parseInput line of
            Run Quit    -> pure s
            Run cmd     -> run cmd s >>= loop
            Prompt text -> turn s text >>= loop

    -- Ctrl-C abandons the turn and keeps the history from before it. GHC
    -- raises UserInterrupt in the main thread; the editor is not reading, so
    -- the terminal sends the signal rather than a key.
    turn s line = handle (\case UserInterrupt -> s <$ say (paint Red "[interrupted]"); e -> throwIO e) $ do
      before <- readIORef spent
      out <- runTurn (env s (fromMaybe 0 (usageCost before))) (sesHistory s) line
      -- Reply text was already streamed by 'liveOutput'.
      case outStop out of
        Finished  -> pure ()
        TurnLimit -> say (paint Red "[stopped: turn limit reached]")
        CostLimit -> say (paint Red "[stopped: cost limit reached]")
        Failed e  -> say (paint Red ("error: " <> e))
      total <- readIORef spent
      when (outTurns out > 0) (cfgRemember cfg (sesModel s))
      say (paint Dim ("[turn: " <> renderUsage (outUsage out) <> " | session: " <> renderUsage total <> " | context: " <> tshow (outContext out) <> "]"))
      persist (outHistory out)
      pure s {sesHistory = outHistory out, sesContext = outContext out, sesCalls = sesCalls s + outTurns out}

    -- A failed save warns: losing the transcript should not end the session.
    persist hist =
      try @IOException (save (drop 1 hist)) >>= \case
        Left e -> say (paint Red ("[session not saved: " <> T.pack (show e) <> "]"))
        Right () -> pure ()

    env s spentCost =
      Env
        { envComplete = cfgComplete cfg
        , envModel = sesModel s
        , envTools = tools s
        , envMode = sesMode s
        , envMaxTurns = cfgMaxTurns cfg
        , envBudget = sesBudget s
        , envCostLimit = cfgCostLimit cfg
        , envSpent = spentCost
        , envHooks =
            Hooks
              { onEvent = \case
                  Narration _ -> pure ()
                  CostUnknown | sesCalls s > 0 -> pure ()
                  ev -> case renderEvent paint ev of
                    Partial t -> TIO.putStr t >> hFlush stdout
                    Full t    -> say t
              -- Ctrl-C here reads as "" and declines the call.
              , confirm = \c -> do
                  say (confirmDetail c)
                  answer <- readlineMaybe (T.unpack (T.dropWhileEnd (`elem` [' ', '>']) (confirmQuestion c)))
                  -- isocline keeps entries over one character; an answer is not a prompt.
                  when (maybe 0 length answer > 1) historyRemoveLast
                  pure (maybe False (isYes . T.pack) answer)
              }
        }

    run cmd s = case cmd of
      Help -> s <$ mapM_ say helpText
      -- Spending stays with the session, so /clear does not reset --max-cost.
      Clear -> do
        persist fresh
        s {sesHistory = fresh, sesContext = 0} <$ say "history cleared"
      SetMode Nothing -> s <$ say ("mode: " <> modeName (sesMode s))
      SetMode (Just m) -> case parseMode m of
        Just mode -> s {sesMode = mode} <$ say ("mode: " <> modeName mode)
        Nothing -> s <$ say (paint Red "modes: yolo, ask, read-only")
      SetModel Nothing -> s <$ say ("model: " <> sesModel s)
      SetModel (Just m) -> do
        budget <- cfgBudget cfg m
        s {sesModel = m, sesBudget = budget} <$ say ("model: " <> m <> ", context budget " <> tshow budget)
      ListTools ->
        s <$ mapM_ (\t -> say (toolName t <> " - " <> toolDescription t)) (visibleTools (sesMode s) (tools s))
      ShowSystem -> s <$ say (cfgSystem cfg)
      ShowUsage -> do
        total <- readIORef spent
        s <$ say ("session: " <> renderUsage total <> " | context: " <> tshow (sesContext s) <> " of " <> tshow (sesBudget s) <> " budget")
      Unknown name -> s <$ say (paint Red ("unknown command /" <> name <> "; try /help"))
      Quit -> pure s

    say t = TIO.putStrLn t >> hFlush stdout
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
  , "Shift+Enter or Ctrl+J        new line in the prompt"
  ]
