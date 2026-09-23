-- | Command-line options, environment resolution and the program entry.
module Hilda.Cli
  ( Options (..)
  , SystemSource (..)
  , AgentsSource (..)
  , optionsInfo
  , resolveProvider
  , main
  ) where

import Control.Monad (mfilter, when)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8Lenient)
import Hilda.App
import Hilda.Policy
import Hilda.Prompt
import Hilda.Provider
import Hilda.Repl (runRepl)
import Hilda.State
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Exit (exitFailure, exitWith)
import System.IO (stderr)

data SystemSource = SystemText Text | SystemFile FilePath
  deriving stock (Eq, Show)

data AgentsSource
  = Discover            -- ^ AGENTS.md from the repository root down to cwd.
  | Explicit [FilePath]
  | NoAgents
  deriving stock (Eq, Show)

data Options = Options
  { optPrompt   :: Maybe Text
  , optOutput   :: Output
  , optProvider :: Maybe Text
  , optModel    :: Maybe Text
  , optBaseUrl  :: Maybe String
  , optKeyEnv   :: Maybe String
  , optMode     :: Mode
  , optSystem   :: Maybe SystemSource
  , optAgents   :: AgentsSource
  , optMaxTurns :: Int
  }
  deriving stock (Eq, Show)

optionsInfo :: ParserInfo Options
optionsInfo =
  info (options <**> helper) $
    fullDesc
      <> progDesc "Coding agent for OpenAI-compatible APIs and OpenRouter. Starts a REPL unless -p is given."
      <> footer
        ( "Provider defaults to openrouter when OPENROUTER_API_KEY is set, else openai. "
            <> "The last model per provider is remembered. Environment: OPENROUTER_API_KEY, OPENAI_API_KEY, OPENAI_BASE_URL."
        )

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
    <*> option
      (maybeReader (parseMode . T.pack))
      (long "mode" <> metavar "yolo|ask|read-only" <> value Yolo <> help "Permission mode (default: yolo)")
    <*> optional
      ( SystemText <$> strOption (long "system" <> metavar "TEXT" <> help "Replace the base system prompt")
          <|> SystemFile <$> strOption (long "system-file" <> metavar "PATH" <> help "Replace the base system prompt with a file")
      )
    <*> ( flag' NoAgents (long "no-agents" <> help "Do not load AGENTS.md")
            <|> Explicit <$> some (strOption (long "agents" <> metavar "PATH" <> help "Load this AGENTS.md instead of discovering one (repeatable)"))
            <|> pure Discover
        )
    <*> option auto (long "max-turns" <> metavar "N" <> value 50 <> showDefault <> help "Model calls allowed per prompt")

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
  pure (Provider kind base key, model)
  where
    nonEmpty = mfilter (not . null) . env

systemPrompt :: Options -> IO Text
systemPrompt o = do
  base <- case optSystem o of
    Nothing             -> pure defaultSystemPrompt
    Just (SystemText t) -> pure t
    Just (SystemFile f) -> readUtf8 f
  cwd <- getCurrentDirectory
  paths <- case optAgents o of
    Discover    -> discoverAgents cwd
    Explicit ps -> pure ps
    NoAgents    -> pure []
  assemble base cwd <$> loadAgents paths

readUtf8 :: FilePath -> IO Text
readUtf8 f = decodeUtf8Lenient <$> BS.readFile f

main :: IO ()
main = do
  o <- execParser optionsInfo
  env <- getEnvironment
  statePath <- stateFile
  models <- loadModels statePath
  (provider, model) <-
    either die' pure (resolveProvider (`lookup` env) (\k -> Map.lookup (kindName k) models) o)
  let remember = rememberModel statePath (providerKind provider)
  remember model
  complete <- newComplete provider >>= either die' pure
  system <- systemPrompt o
  let cfg =
        Config
          { cfgComplete = complete
          , cfgProvider = providerKind provider
          , cfgModel = model
          , cfgMode = optMode o
          , cfgSystem = system
          , cfgMaxTurns = optMaxTurns o
          , cfgRemember = remember
          }
  case optPrompt o of
    Just "-" -> decodeUtf8Lenient <$> BS.getContents >>= runHeadless cfg (optOutput o) >>= exitWith
    Just p -> runHeadless cfg (optOutput o) p >>= exitWith
    Nothing
      | optOutput o /= Text -> die' "--json and --stream-json need a prompt (-p)"
      | otherwise -> runRepl cfg
  where
    die' msg = TIO.hPutStrLn stderr ("hilda: " <> msg) >> exitFailure
