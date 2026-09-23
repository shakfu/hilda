-- | Interactive session. Each line is a prompt or a slash command; the
-- session state is a value threaded through the input loop.
module Hilda.Repl
  ( Command (..)
  , parseCommand
  , runRepl
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
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

-- | 'Nothing' when the line is a prompt rather than a command.
parseCommand :: Text -> Maybe Command
parseCommand line = case T.words line of
  (w : rest) | Just name <- T.stripPrefix "/" w -> Just (command name (listToMaybe rest))
  _ -> Nothing
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

data Session = Session
  { sesMode    :: Mode
  , sesModel   :: Text
  , sesHistory :: [Message]
  , sesUsage   :: Usage
  , sesContext :: Int -- ^ Prompt tokens of the last model call.
  }

-- | Streams reply text to stdout, with a waiting line before it on a
-- terminal.
runRepl :: Config -> IO ()
runRepl cfg0 = do
  terminal <- ansiTerminal stdout
  paint <- (\on -> if on then ansi else plain) <$> colorEnabled stdout
  interactive paint cfg0 {cfgComplete = liveOutput stdout paint (Live terminal True) (cfgComplete cfg0)}

interactive :: Paint -> Config -> IO ()
interactive paint cfg = do
  dir <- getXdgDirectory XdgState "hilda"
  createDirectoryIfMissing True dir
  runInputT defaultSettings {historyFile = Just (dir </> "history")} $ do
    say (paint BoldMagenta versionText)
    say (paint Dim ("model " <> cfgModel cfg <> ", mode " <> modeName (cfgMode cfg) <> ". /help for commands, Ctrl-D to exit."))
    -- The prompt stays uncolored: escape codes in it break haskeline's
    -- cursor arithmetic.
    final <- loop (Session (cfgMode cfg) (cfgModel cfg) fresh mempty 0)
    say (paint Dim ("session: " <> renderUsage (sesUsage final)))
  where
    fresh = [System (cfgSystem cfg)]

    loop s =
      getInputLine "hilda> " >>= \case
        Nothing -> pure s
        Just raw -> case T.strip (T.pack raw) of
          "" -> loop s
          line -> case parseCommand line of
            Just Quit -> pure s
            Just cmd  -> run cmd s >>= loop
            Nothing   -> turn s line >>= loop

    -- Ctrl-C abandons the turn and keeps the history from before it.
    turn s line = handleInterrupt (say (paint Red "[interrupted]") >> pure s) . withInterrupt $ do
      out <- runTurn (env s) (sesHistory s) line
      -- Reply text was already streamed by 'liveOutput'.
      case outStop out of
        Finished  -> pure ()
        TurnLimit -> say (paint Red "[stopped: turn limit reached]")
        Failed e  -> say (paint Red ("error: " <> e))
      let total = sesUsage s <> outUsage out
      say (paint Dim ("[turn: " <> renderUsage (outUsage out) <> " | session: " <> renderUsage total <> " | context: " <> tshow (outContext out) <> "]"))
      pure s {sesHistory = outHistory out, sesUsage = total, sesContext = outContext out}

    env s =
      Env
        { envComplete = cfgComplete cfg
        , envModel = sesModel s
        , envTools = builtinTools
        , envMode = sesMode s
        , envMaxTurns = cfgMaxTurns cfg
        , envBudget = cfgBudget cfg
        , envHooks =
            Hooks
              { onEvent = \case
                  Narration _ -> pure ()
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
      Clear -> s {sesHistory = fresh, sesUsage = mempty, sesContext = 0} <$ say "history cleared"
      SetMode Nothing -> s <$ say ("mode: " <> modeName (sesMode s))
      SetMode (Just m) -> case parseMode m of
        Just mode -> s {sesMode = mode} <$ say ("mode: " <> modeName mode)
        Nothing -> s <$ say (paint Red "modes: yolo, ask, read-only")
      SetModel Nothing -> s <$ say ("model: " <> sesModel s)
      SetModel (Just m) -> do
        liftIO (cfgRemember cfg m)
        s {sesModel = m} <$ say ("model: " <> m)
      ListTools ->
        s <$ mapM_ (\t -> say (toolName t <> " - " <> toolDescription t)) (visibleTools (sesMode s) builtinTools)
      ShowSystem -> s <$ say (cfgSystem cfg)
      ShowUsage ->
        s <$ say ("session: " <> renderUsage (sesUsage s) <> " | context: " <> tshow (sesContext s) <> " of " <> tshow (cfgBudget cfg) <> " budget")
      Unknown name -> s <$ say (paint Red ("unknown command /" <> name <> "; try /help"))
      Quit -> pure s

    say = outputStrLn . T.unpack
    tshow = T.pack . show

helpText :: [Text]
helpText =
  [ "/mode [yolo|ask|read-only]  show or set the permission mode"
  , "/model [name]               show or set the model"
  , "/tools                      list tools available in this mode"
  , "/system                     print the system prompt"
  , "/usage                      tokens and cost for this session"
  , "/clear                      start a new conversation"
  , "/quit                       exit (or Ctrl-D)"
  ]
