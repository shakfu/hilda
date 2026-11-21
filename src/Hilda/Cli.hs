-- | Command-line options, environment resolution and the program entry.
module Hilda.Cli
  ( Options (..)
  , SystemSource (..)
  , AgentsSource (..)
  , Start (..)
  , optionsInfo
  , start
  , resolveProvider
  , contextBudget
  , modeOf
  , main
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, mfilter, void, when)
import Data.Bifunctor (first)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8Lenient)
import Hilda.Acp (runAcp)
import Hilda.App
import Hilda.Policy
import Hilda.Prompt
import Hilda.Provider
import Hilda.Render (When (..), parseWhen)
import Hilda.Repl (runRepl)
import Hilda.State
import Options.Applicative
import Options.Applicative.Help.Pretty (pretty, vsep)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Environment (getEnvironment)
import System.Exit (exitFailure, exitWith)
import System.IO (hPutStrLn, stderr)

-- | Instructions given inline or as a file path.
data SystemSource = SystemText Text | SystemFile FilePath
  deriving stock (Eq, Show)

-- | Which AGENTS.md files to load.
data AgentsSource
  = Discover            -- ^ AGENTS.md from the repository root down to cwd.
  | Explicit [FilePath] -- ^ These files, from @--agents@.
  | NoAgents            -- ^ None, from @--no-agents@.
  deriving stock (Eq, Show)

-- | Parsed command-line options, before environment resolution.
data Options = Options
  { optPrompt   :: Maybe Text
  , optOutput   :: Output
  , optProvider :: Maybe Text
  , optModel    :: Maybe Text
  , optBaseUrl  :: Maybe String
  , optKeyEnv   :: Maybe String
  , optMode     :: Maybe Mode -- ^ Absent: see 'modeOf'.
  , optSystem   :: Maybe SystemSource
  , optAppend   :: Maybe SystemSource
  , optAgents   :: AgentsSource
  , optMaxTurns :: Int
  , optBudget   :: Maybe Int -- ^ Absent: see 'contextBudget'.
  , optWindow   :: Maybe Int -- ^ The model's context window; absent: looked up on OpenRouter.
  , optCostLimit :: Maybe Double
  , optKeepReasoning :: Bool
  , optEffort   :: Maybe Text
  , optContinue :: Bool
  , optResume   :: Maybe Text
  , optNoSession :: Bool
  , optNoColor  :: Bool
  , optMarkdown :: When
  , optAcp      :: Bool
  , optWords    :: [Text] -- ^ Positional PROMPT words.
  }
  deriving stock (Eq, Show)

-- | The option parser, with @--help@, @--version@ and the help footer.
optionsInfo :: ParserInfo Options
optionsInfo =
  info (options <**> versionFlag <**> helper) $
    fullDesc
      <> progDesc "Coding agent for OpenAI-compatible APIs and OpenRouter. Starts a REPL unless -p is given; PROMPT... becomes the REPL's first entry."
      <> footerDoc
        ( Just . vsep . map pretty $
            [ "Provider: openrouter when OPENROUTER_API_KEY is set, else openai."
            , "Models: the last model per provider is saved in"
            , "  $XDG_STATE_HOME/hilda/models.json (default ~/.local/state/hilda/)."
            , "Environment: OPENROUTER_API_KEY, OPENAI_API_KEY, OPENAI_BASE_URL,"
            , "  NO_COLOR (disables color and markdown styling)."
            , "Exit codes: 0 finished, 1 error, 2 stopped by --max-turns, 3 stopped by --max-cost." :: String
            ]
        )
  where
    versionFlag = infoOption (T.unpack versionText) (short 'V' <> long "version" <> help "Show version information")

-- | Every option except @--help@ and @--version@.
options :: Parser Options
options =
  Options
    <$> optional (strOption (short 'p' <> long "prompt" <> metavar "TEXT" <> help "Run one prompt headlessly and exit. '-' reads stdin."))
    <*> ( flag' Json (long "json" <> help "Print the outcome as one JSON object (needs -p)")
            <|> flag' StreamJson (long "stream-json" <> help "Print one JSON object per event, then the outcome (needs -p)")
            <|> pure Text
        )
    <*> optional (strOption (short 'P' <> long "provider" <> metavar "openai|openrouter" <> help "Backend; openai means any OpenAI-compatible server"))
    <*> optional (strOption (short 'm' <> long "model" <> metavar "NAME" <> help "Model name (default: the last one used with this provider)"))
    <*> optional (strOption (long "base-url" <> metavar "URL" <> help "API base URL, e.g. http://localhost:11434/v1"))
    <*> optional (strOption (long "api-key-env" <> metavar "VAR" <> help "Read the API key from this environment variable"))
    <*> optional
      ( option
          (maybeReader (parseMode . T.pack))
          ( short 'M' <> long "mode" <> metavar "yolo|ask|read-only"
              <> help "Permission mode. yolo: run all tools (default; ask under --acp). ask: confirm write, edit and bash. read-only: offer only read."
          )
      )
    <*> optional
      ( SystemText <$> strOption (long "system" <> metavar "TEXT" <> help "Replace hilda's default instructions (AGENTS.md still loads)")
          <|> SystemFile <$> strOption (long "system-file" <> metavar "PATH" <> help "Replace hilda's default instructions with a file (AGENTS.md still loads)")
      )
    <*> optional
      ( SystemText <$> strOption (long "append-system" <> metavar "TEXT" <> help "Add instructions after hilda's default ones (or after --system)")
          <|> SystemFile <$> strOption (long "append-system-file" <> metavar "PATH" <> help "Add instructions from a file after hilda's default ones (or after --system)")
      )
    <*> ( flag' NoAgents (long "no-agents" <> help "Do not load AGENTS.md")
            <|> Explicit <$> some (strOption (long "agents" <> metavar "PATH" <> help "Load this AGENTS.md instead of discovering one (repeatable)"))
            <|> pure Discover
        )
    <*> option positive (long "max-turns" <> metavar "N" <> value 50 <> showDefault <> help "Model calls allowed per prompt")
    <*> optional
      ( option
          positive
          ( long "context-budget" <> metavar "TOKENS"
              <> help "Elide the oldest tool results when the history exceeds this many tokens (estimated at 4 characters each). Default: half the model's context window on OpenRouter, at most 100000; else 100000"
          )
      )
    <*> optional
      ( option
          positive
          ( long "context-window" <> metavar "TOKENS"
              <> help "The model's context window, for the default --context-budget and ACP usage_update. Default: looked up on OpenRouter; else unknown"
          )
      )
    <*> optional
      ( option
          positive
          (long "max-cost" <> metavar "USD" <> help "Stop before the next model call once the prompt (REPL: session) has cost this much. Needs a provider that reports cost")
      )
    <*> switch
      ( long "keep-reasoning"
          <> help "Send reasoning_details back with later requests, as OpenRouter advises for tool calls with reasoning models"
      )
    <*> optional
      ( option
          (maybeReader (\s -> let e = T.toLower (T.pack s) in if e `elem` efforts then Just e else Nothing))
          ( long "reasoning" <> metavar "EFFORT"
              <> help ("Ask the model to reason: " <> T.unpack (T.intercalate "|" efforts) <> ". Models support different subsets")
          )
      )
    <*> switch (short 'c' <> long "continue" <> help "Resume the last REPL session in this directory")
    <*> optional (strOption (long "resume" <> metavar "ID" <> help "Resume the session with this id, from a -p run's session_id (REPL or -p)"))
    <*> switch (long "no-session" <> help "Save no transcript: a -p run reports no session_id, and the REPL keeps no history for -c. --resume and -c still read one")
    <*> switch (long "no-color" <> help "Do not color status and tool lines. NO_COLOR also turns off markdown styling")
    <*> option
      (maybeReader (parseWhen . T.pack))
      ( long "markdown" <> metavar "always|auto|never" <> value Auto
          <> help "Style reply text as markdown. auto: on a terminal without NO_COLOR (default)"
      )
    <*> switch (long "acp" <> help "Serve the Agent Client Protocol (version 1) on stdin and stdout, for editors and brokers")
    <*> many
      ( strArgument
          ( metavar "PROMPT..."
              <> help "Start the REPL and enter these words, joined by spaces, as if typed. A leading / runs a command; // sends a prompt starting with /. Put -- before a prompt that starts with -"
          )
      )
  where
    positive :: (Read a, Num a, Ord a) => ReadM a
    positive = auto >>= \n -> if n > 0 then pure n else readerError "must be positive"

-- | The permission mode: @-M@, else ask under @--acp@, where the client
-- answers permission requests, else yolo.
modeOf :: Options -> Mode
modeOf o = fromMaybe (if optAcp o then Ask else Yolo) (optMode o)

-- | What 'main' runs.
data Start
  = Headless (Maybe Text)    -- ^ @-p@: the prompt, or Nothing to read stdin.
  | Interactive (Maybe Text) -- ^ The REPL, with PROMPT... as its first entry.
  | Acp                      -- ^ @--acp@: an ACP server on stdin and stdout.
  deriving stock (Eq, Show)

-- | Choose what to run, or say which options conflict.
start :: Options -> Either Text Start
start o
  | optAcp o = acp
  | otherwise = case (optPrompt o, optWords o) of
    (Just _, _ : _) -> Left "PROMPT... starts the REPL; it cannot be used with -p"
    _ | optContinue o && isJust (optResume o) -> Left "--continue and --resume both choose a session; pass one"
    (Just _, []) | optContinue o -> Left "--continue resumes the REPL; use --resume ID with -p"
    (Just "-", []) -> Right (Headless Nothing)
    (Just p, []) -> Right (Headless (Just p))
    (Nothing, ws)
      | optOutput o /= Text -> Left "--json and --stream-json need a prompt (-p); PROMPT... starts the REPL"
      | null ws -> Right (Interactive Nothing)
      | otherwise -> Right (Interactive (Just (T.unwords ws)))
  where
    acp
      | isJust (optPrompt o) || optOutput o /= Text || not (null (optWords o)) =
          Left "--acp serves ACP on stdin and stdout; it cannot be used with -p, --json, --stream-json or PROMPT..."
      | optContinue o || isJust (optResume o) = Left "--acp: the client picks the session with session/load; drop --continue and --resume"
      | optNoSession o = Left "--acp saves each session so session/load can find it; drop --no-session"
      | otherwise = Right Acp

-- | Pick the backend, key and model. Pure over the environment lookup and
-- the remembered models, so it is testable.
resolveProvider
  :: (String -> Maybe String) -- ^ Environment lookup.
  -> (ProviderKind -> Maybe Text) -- ^ Last model used with a provider.
  -> Options
  -> Either Text (Provider, Text)
resolveProvider env remembered o = do
  kind <- case optProvider o of
    Just t -> maybe (Left ("unknown provider '" <> t <> "' (expected openai or openrouter)")) Right (parseKind t)
    Nothing
      | isJust (nonEmpty (keyVariable OpenRouter)) -> Right OpenRouter
      | otherwise -> Right OpenAICompatible
  model <-
    maybe (Left ("no model for " <> kindName kind <> ": pass -m NAME once")) Right $
      optModel o <|> remembered kind
  let keyVar = fromMaybe (keyVariable kind) (optKeyEnv o)
      key = T.pack <$> nonEmpty keyVar
      baseEnv = case kind of
        OpenAICompatible -> nonEmpty "OPENAI_BASE_URL"
        OpenRouter       -> Nothing
      base = fromMaybe (defaultBaseUrl kind) (optBaseUrl o <|> baseEnv)
  -- Local OpenAI-compatible servers often need no key; OpenRouter always does.
  when (isNothing key && (kind == OpenRouter || isJust (optKeyEnv o))) $
    Left ("no API key in $" <> T.pack keyVar)
  pure (Provider kind base key (optEffort o), model)
  where
    nonEmpty = mfilter (not . null) . env

-- | Read the instruction sources and the AGENTS.md files for @cwd@ and
-- assemble the system prompt.
systemPrompt :: Options -> FilePath -> IO Text
systemPrompt o cwd = do
  instructions <- maybe (pure defaultSystemPrompt) readSource (optSystem o)
  extra <- traverse readSource (optAppend o)
  paths <- case optAgents o of
    Discover    -> discoverAgents cwd
    Explicit ps -> pure ps
    NoAgents    -> pure []
  assemble (instructions : maybeToList extra) cwd <$> loadAgents paths
  where
    readSource = \case
      SystemText t -> pure t
      SystemFile f -> readUtf8 f

-- | The options with @--system-file@, @--append-system-file@ and
-- @--agents@ paths made absolute against the current directory.
absolutePaths :: Options -> IO Options
absolutePaths o = do
  sys <- traverse source (optSystem o)
  extra <- traverse source (optAppend o)
  agents <- case optAgents o of
    Explicit ps -> Explicit <$> traverse makeAbsolute ps
    a -> pure a
  pure o {optSystem = sys, optAppend = extra, optAgents = agents}
  where
    source = \case
      SystemFile f -> SystemFile <$> makeAbsolute f
      t -> pure t

-- | Read a file as UTF-8, replacing invalid bytes.
readUtf8 :: FilePath -> IO Text
readUtf8 f = decodeUtf8Lenient <$> BS.readFile f

-- | The model's context window on OpenRouter, cached in 'contextsFile'.
-- Other providers have no common way to report it.
knownContext :: Provider -> Text -> IO (Maybe Int)
knownContext p model
  | providerKind p /= OpenRouter = pure Nothing
  | otherwise = do
      path <- contextsFile
      Map.lookup model <$> loadMap path >>= \case
        Just n -> pure (Just n)
        Nothing -> do
          found <- lookupContext p model
          -- A failed cache write costs a lookup next run, not this one.
          forM_ found $ \n ->
            try @IOException (remember path model n)
              >>= either (\e -> hPutStrLn stderr ("hilda: context window not cached: " <> show e)) pure
          pure found

-- | Resolve options and environment, then run headless or start the REPL.
main :: IO ()
main = do
  o <- execParser optionsInfo
  what <- either die' pure (start o)
  let unlessNoSession write = if optNoSession o then Nothing else Just write
  env <- getEnvironment
  -- Once a run, so old transcripts of file contents and command output go.
  case sessionDays (lookup "HILDA_SESSION_DAYS" env) of
    Left why -> TIO.hPutStrLn stderr ("hilda: " <> why <> "; no session was deleted")
    Right days -> void (pruneIdSessions days)
  statePath <- stateFile
  models <- loadMap statePath
  (provider, model) <-
    either die' pure (resolveProvider (`lookup` env) (\k -> Map.lookup (kindName k) models) o)
  let saveModel = rememberModel statePath (providerKind provider)
  complete <- (if optKeepReasoning o then id else withoutReasoning) <$> (newComplete provider >>= either die' pure)
  let windowFor = maybe (knownContext provider) (const . pure . Just) (optWindow o)
      budgetWith window = maybe (fmap contextBudget . window) (const . pure) (optBudget o)
      budgetFor = budgetWith windowFor
  cwd <- getCurrentDirectory
  system <- systemPrompt o cwd
  sessionPath <- sessionFile cwd
  let cfg =
        Config
          { cfgComplete = complete
          , cfgProvider = providerKind provider
          , cfgModel = model
          , cfgMode = modeOf o
          , cfgSystem = system
          , cfgMaxTurns = optMaxTurns o
          , cfgBudget = budgetFor
          , cfgWindow = windowFor
          , cfgCostLimit = optCostLimit o
          , cfgRemember = saveModel
          , cfgColor = not (optNoColor o)
          , cfgMarkdown = optMarkdown o
          }
  -- A session chosen by id, its file and history.
  let resume sid =
        first (("--resume " <> sid <> ": ") <>) <$> (idSessionFile sid >>= \case
          Left why -> pure (Left why)
          Right path -> fmap ((,) path) <$> loadIdSession path cwd)
  case what of
    Headless input -> do
      prompt <- maybe (decodeUtf8Lenient <$> BS.getContents) pure input
      session <- case optResume o of
        Nothing -> do
          sid <- newSessionId
          fmap (\path -> (sid, path, ([], Nothing))) <$> idSessionFile sid
        Just sid -> fmap (\(path, msgs) -> (sid, path, msgs)) <$> resume sid
      case session of
        Left why -> headlessFailure cfg (optOutput o) why >>= exitWith
        Right (sid, path, (msgs, cost)) -> runHeadless cfg (optOutput o) (Saved sid msgs (unlessNoSession (saveIdSession path cwd cost))) prompt >>= exitWith
    Interactive initial -> do
      (restored, save) <- case optResume o of
        Just sid -> resume sid >>= either die' (\(path, (msgs, cost)) -> pure (msgs, saveIdSession path cwd cost))
        Nothing -> do
          restored <-
            if optContinue o
              then loadSession sessionPath >>= either (\why -> [] <$ TIO.hPutStrLn stderr ("hilda: " <> why <> "; starting fresh")) pure
              else pure []
          pure (restored, saveSession sessionPath)
      runRepl cfg initial restored (fromMaybe (const (pure ())) (unlessNoSession save))
    -- Resolved now: ACP prompts change the working directory.
    Acp -> do
      -- ACP asks for both the window and the budget; one lookup serves both.
      window <- memo windowFor
      absolutePaths o >>= runAcp cfg {cfgWindow = window, cfgBudget = budgetWith window} . systemPrompt
  where
    die' msg = TIO.hPutStrLn stderr ("hilda: " <> msg) >> exitFailure

-- | @f@, with each key's result kept after its first call.
memo :: (Text -> IO a) -> IO (Text -> IO a)
memo f = do
  seen <- newIORef Map.empty
  pure $ \k ->
    Map.lookup k <$> readIORef seen >>= \case
      Just v -> pure v
      Nothing -> f k >>= \v -> v <$ modifyIORef' seen (Map.insert k v)
