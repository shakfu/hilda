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
import Hilda.Tools
import Hilda.Types
import System.Console.Haskeline
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

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
  }

runRepl :: Config -> IO ()
runRepl cfg = do
  dir <- getXdgDirectory XdgState "hilda"
  createDirectoryIfMissing True dir
  runInputT defaultSettings {historyFile = Just (dir </> "history")} $ do
    say ("hilda: " <> cfgModel cfg <> ", mode " <> modeName (cfgMode cfg) <> ". /help for commands, Ctrl-D to exit.")
    loop (Session (cfgMode cfg) (cfgModel cfg) fresh mempty)
  where
    fresh = [System (cfgSystem cfg)]

    loop s =
      getInputLine "hilda> " >>= \case
        Nothing -> pure ()
        Just raw -> case T.strip (T.pack raw) of
          "" -> loop s
          line -> case parseCommand line of
            Just Quit -> pure ()
            Just cmd  -> run cmd s >>= loop
            Nothing   -> turn s line >>= loop

    -- Ctrl-C abandons the turn and keeps the history from before it.
    turn s line = handleInterrupt (say "[interrupted]" >> pure s) . withInterrupt $ do
      out <- runTurn (env s) (sesHistory s) line
      case outStop out of
        Finished  -> say (outText out)
        TurnLimit -> say "[stopped: turn limit reached]"
        Failed e  -> say ("error: " <> e)
      pure s {sesHistory = outHistory out, sesUsage = sesUsage s <> outUsage out}

    env s =
      Env
        { envComplete = cfgComplete cfg
        , envModel = sesModel s
        , envTools = builtinTools
        , envMode = sesMode s
        , envMaxTurns = cfgMaxTurns cfg
        , envHooks =
            Hooks
              { onEvent = say . renderEvent
              , confirm = \c -> maybe False (isYes . T.pack) <$> getInputLine (T.unpack (confirmQuestion c))
              }
        }

    run cmd s = case cmd of
      Help -> s <$ mapM_ say helpText
      Clear -> s {sesHistory = fresh, sesUsage = mempty} <$ say "history cleared"
      SetMode Nothing -> s <$ say ("mode: " <> modeName (sesMode s))
      SetMode (Just m) -> case parseMode m of
        Just mode -> s {sesMode = mode} <$ say ("mode: " <> modeName mode)
        Nothing -> s <$ say "modes: yolo, ask, read-only"
      SetModel Nothing -> s <$ say ("model: " <> sesModel s)
      SetModel (Just m) -> do
        liftIO (cfgRemember cfg m)
        s {sesModel = m} <$ say ("model: " <> m)
      ListTools ->
        s <$ mapM_ (\t -> say (toolName t <> " - " <> toolDescription t)) (visibleTools (sesMode s) builtinTools)
      ShowSystem -> s <$ say (cfgSystem cfg)
      ShowUsage ->
        s <$ say ("tokens: " <> tshow (usagePrompt (sesUsage s)) <> " in, " <> tshow (usageCompletion (sesUsage s)) <> " out")
      Unknown name -> s <$ say ("unknown command /" <> name <> "; try /help")
      Quit -> pure s

    say = outputStrLn . T.unpack
    tshow = T.pack . show

helpText :: [Text]
helpText =
  [ "/mode [yolo|ask|read-only]  show or set the permission mode"
  , "/model [name]               show or set the model"
  , "/tools                      list tools available in this mode"
  , "/system                     print the system prompt"
  , "/usage                      token totals for this session"
  , "/clear                      start a new conversation"
  , "/quit                       exit (or Ctrl-D)"
  ]
