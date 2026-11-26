-- | Agent Client Protocol (ACP) version 1 on stdin and stdout
-- (<https://agentclientprotocol.com/protocol/>), so an editor or a broker
-- can drive hilda as a subprocess. It is a front end beside the REPL and
-- headless runs: each prompt is one 'runTurn' with ACP hooks.
--
-- The main thread reads stdin, so @session/cancel@ and answers to
-- @session/request_permission@ arrive while a prompt runs in its own
-- thread. Prompts run one at a time, since tools resolve paths against the
-- process's working directory. Sessions are the id sessions of
-- "Hilda.State", so @hilda --resume ID@ continues a session an ACP client
-- created, and @session/load@ loads one a @-p@ run saved.
module Hilda.Acp
  ( runAcp
  , stopReason
  , toolKind
  , promptText
  , replay
  ) where

import Control.Concurrent
import Control.Exception (IOException, finally, mask_, try, uninterruptibleMask_)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair, Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isSpace)
import Data.List (find, mapAccumL)
import Data.IORef
import Data.Unique (Unique, newUnique)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Hilda.Agent
import Hilda.App (Config (..))
import Hilda.Context (contextBudget)
import Hilda.Family (attach)
import Hilda.Policy (Verdict (..), authorize)
import Hilda.Provider (ProviderKind (..))
import Hilda.Render (Line (..), callSummary, plain, renderEvent)
import Hilda.State (idSessionFile, loadIdSession, newSessionId, saveIdSession)
import Hilda.Tools (Tool (..), builtinTools, resultLimitFor)
import Hilda.Types
import Paths_hilda (version)
import System.Directory (canonicalizePath, doesDirectoryExist, setCurrentDirectory)
import System.FilePath (isAbsolute, (</>))
import System.IO

-- | State shared by the reader thread and the prompt threads.
data Server = Server
  { srvCfg      :: Config
  , srvSystem   :: FilePath -> IO Text -- ^ System prompt for a working directory.
  , srvOut      :: MVar () -- ^ Held while one line is written to stdout.
  , srvNextId   :: IORef Int -- ^ Id of the next request to the client.
  , srvPending  :: IORef (Map Int (MVar Value)) -- ^ Requests to the client awaiting an answer.
  , srvSessions :: IORef (Map Text Session)
  , srvReady    :: IORef Bool -- ^ @initialize@ was answered.
  , srvTurn     :: MVar () -- ^ Held while a prompt runs.
  , srvLimits   :: MVar (Int, Maybe Int) -- ^ Context budget and window, filled once looked up.
  , srvAnswers  :: IORef (Map Unique (MVar ())) -- ^ Prompts not yet answered; each filled once answered.
  }

-- | One ACP session.
data Session = Session
  { sesId      :: Text
  , sesCwd     :: FilePath
  , sesFile    :: FilePath
  , sesSystem  :: Text
  , sesHistory :: IORef [Message] -- ^ Without the system prompt.
  , sesSpent   :: IORef Usage -- ^ Every model call, cancelled turns included.
  , sesContext :: IORef Int -- ^ Prompt tokens of the last finished turn.
  , sesRules   :: IORef (Map Text Bool) -- ^ Tool name to an "always" answer.
  , sesRunning :: IORef (Maybe (IO ())) -- ^ Cancel the running prompt.
  , sesReplies :: IORef Int -- ^ Model replies so far; numbers ACP tool call ids.
  }

-- | A JSON-RPC error: code and message.
type Failure = (Int, Text)

-- | JSON-RPC 2.0 and ACP error codes.
parseError, invalidRequest, methodNotFound, invalidParams, internalError, notFound :: Int
parseError = -32700
invalidRequest = -32600
methodNotFound = -32601
invalidParams = -32602
internalError = -32603
notFound = -32002

-- | Serve ACP until stdin closes. @system@ builds the system prompt for a
-- session's working directory. At end of input, running prompts are
-- cancelled and answered before it returns.
runAcp :: Config -> (FilePath -> IO Text) -> IO ()
runAcp cfg system = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  srv <-
    Server cfg system
      <$> newMVar ()
      <*> newIORef 0
      <*> newIORef Map.empty
      <*> newIORef Map.empty
      <*> newIORef False
      <*> newMVar ()
      <*> newEmptyMVar
      <*> newIORef Map.empty
  -- Looked up off the reader thread, so a slow lookup never delays a cancel.
  -- Filled even if the lookup throws, or every prompt would wait forever.
  let limits = \case
        Right l -> putMVar (srvLimits srv) l
        Left e -> do
          hPutStrLn stderr ("hilda: context window lookup failed: " <> show e)
          putMVar (srvLimits srv) (contextBudget Nothing, Nothing)
  void . flip forkFinally limits $ do
    window <- cfgWindow cfg (cfgModel cfg)
    budget <- cfgBudget cfg (cfgModel cfg)
    pure (budget, window)
  let loop =
        try @IOException (BS8.hGetLine stdin) >>= \case
          Left _ -> pure ()
          Right l -> handleLine srv (BS8.dropWhileEnd (== '\r') l) >> loop
  loop
  sessions <- Map.elems <$> readIORef (srvSessions srv)
  forM_ sessions $ \s -> readIORef (sesRunning s) >>= sequence_
  readIORef (srvAnswers srv) >>= mapM_ readMVar

-- | Write one message as one line. A closed stdout drops it: nobody reads.
send :: Server -> Value -> IO ()
send srv v =
  -- Uninterruptible, so a cancelled prompt never leaves half a line.
  uninterruptibleMask_ . withMVar (srvOut srv) $ \_ ->
    void (try @IOException (BL.hPut stdout (encode v <> "\n") >> hFlush stdout))

-- | Answer request @i@.
respond :: Server -> Value -> Value -> IO ()
respond srv i r = send srv (object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "result" .= r])

-- | Answer request @i@ with an error.
failWith :: Server -> Value -> Failure -> IO ()
failWith srv i (code, msg) =
  send srv (object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "error" .= object ["code" .= code, "message" .= msg]])

-- | Send a @session/update@ for session @sid@.
update :: Server -> Text -> Value -> IO ()
update srv sid u =
  send srv (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("session/update" :: Text), "params" .= object ["sessionId" .= sid, "update" .= u]])

-- | Send a request to the client and wait for its whole response message.
ask :: Server -> Text -> Value -> IO Value
ask srv method params = do
  n <- atomicModifyIORef' (srvNextId srv) (\k -> (k + 1, k))
  box <- newEmptyMVar
  atomicModifyIORef' (srvPending srv) (\m -> (Map.insert n box m, ()))
  (send srv (object ["jsonrpc" .= ("2.0" :: Text), "id" .= n, "method" .= method, "params" .= params]) >> takeMVar box)
    `finally` atomicModifyIORef' (srvPending srv) (\m -> (Map.delete n m, ()))

-- | Handle one line of input.
handleLine :: Server -> BS.ByteString -> IO ()
handleLine srv line
  | BS8.all isSpace line = pure ()
  | otherwise = case eitherDecodeStrict line of
      Left _ -> failWith srv Null (parseError, "Parse error")
      Right (Object o) -> case (KM.lookup "method" o, KM.lookup "id" o) of
        (Just (String m), Just i) -> request srv i m (param o)
        (Just (String m), Nothing) -> notification srv m (param o)
        (Nothing, Just i) | KM.member "result" o || KM.member "error" o -> answered i (Object o)
        (_, i) -> failWith srv (fromMaybe Null i) (invalidRequest, "Invalid Request")
      Right _ -> failWith srv Null (invalidRequest, "Invalid Request: not an object")
  where
    param = fromMaybe Null . KM.lookup "params"
    answered i v = case fromJSON i of
      Success n -> readIORef (srvPending srv) >>= mapM_ (\box -> void (tryPutMVar box v)) . Map.lookup (n :: Int)
      Error _ -> pure ()

-- | Notifications from the client. Only @session/cancel@ has an effect.
notification :: Server -> Text -> Value -> IO ()
notification srv m params = case m of
  "session/cancel"
    | Right sid <- parseEither (withObject "params" (.: "sessionId")) params ->
        readIORef (srvSessions srv) >>= mapM_ (\s -> readIORef (sesRunning s) >>= sequence_) . Map.lookup sid
  _ -> pure ()

-- | Requests from the client.
request :: Server -> Value -> Text -> Value -> IO ()
request srv i m params = do
  ready <- readIORef (srvReady srv)
  case m of
    "initialize" -> withParams (withObject "params" (\o -> o .: "protocolVersion" :: Parser Int)) $ \_ ->
      Right initialized <$ writeIORef (srvReady srv) True
    _ | not ready -> failWith srv i (invalidRequest, "Invalid Request: send initialize first")
    "session/new" -> withParams (withObject "params" (\o -> (,) <$> o .: "cwd" <*> o .:? "mcpServers" .!= [])) $ \(cwd, mcp) -> do
      ignoreMcp mcp
      directory cwd >>= \case
        Left f -> pure (Left f)
        Right dir -> do
          sid <- newSessionId
          idSessionFile sid >>= \case
            Left why -> pure (Left (internalError, why))
            Right path -> do
              s <- newSession srv sid path dir [] Nothing
              -- Saved now, so session/load finds a session never prompted.
              persist s
              Right (object ["sessionId" .= sid]) <$ atomicModifyIORef' (srvSessions srv) (\ss -> (Map.insert sid s ss, ()))
    "session/load" -> withParams (withObject "params" (\o -> (,,) <$> o .: "sessionId" <*> o .: "cwd" <*> o .:? "mcpServers" .!= [])) $ \(sid, cwd, mcp) -> do
      ignoreMcp mcp
      running <- maybe (pure False) (fmap isJust . readIORef . sesRunning) . Map.lookup sid =<< readIORef (srvSessions srv)
      if running
        then pure (Left (invalidRequest, "a prompt is running in session " <> sid))
        else
          directory cwd >>= \case
            Left f -> pure (Left f)
            Right dir ->
              idSessionFile sid >>= \case
                Left why -> pure (Left (notFound, why))
                Right path ->
                  loadIdSession path dir >>= \case
                    Left why -> pure (Left (notFound, why))
                    Right (msgs, cost) -> do
                      -- The history goes out before the response, as session/load requires.
                      mapM_ (update srv sid) (replay dir msgs)
                      s <- newSession srv sid path dir msgs cost
                      Right (object []) <$ atomicModifyIORef' (srvSessions srv) (\ss -> (Map.insert sid s ss, ()))
    "session/prompt" ->
      case parseEither (withObject "params" (\o -> (,) <$> o .: "sessionId" <*> o .: "prompt")) params of
        Left e -> failWith srv i (invalidParams, "Invalid params: " <> T.pack e)
        Right (sid, blocks) ->
          Map.lookup sid <$> readIORef (srvSessions srv) >>= \case
            Nothing -> failWith srv i (notFound, "no session " <> sid)
            Just s -> case promptText blocks of
              Left e -> failWith srv i (invalidParams, e)
              Right text ->
                readIORef (sesRunning s) >>= \case
                  Just _ -> failWith srv i (invalidRequest, "a prompt is already running in this session")
                  Nothing -> startPrompt srv s i text
    _ -> failWith srv i (methodNotFound, "Method not found: " <> m)
  where
    withParams :: (Value -> Parser a) -> (a -> IO (Either Failure Value)) -> IO ()
    withParams p k = case parseEither p params of
      Left e -> failWith srv i (invalidParams, "Invalid params: " <> T.pack e)
      Right a ->
        try @IOException (k a) >>= \case
          Left e -> failWith srv i (internalError, T.pack (show e))
          Right r -> either (failWith srv i) (respond srv i) r
    ignoreMcp :: [Value] -> IO ()
    ignoreMcp mcp = unless (null mcp) $
      hPutStrLn stderr ("hilda: ignoring " <> show (length mcp) <> " MCP server(s): hilda has no MCP client")

-- | The @initialize@ result. hilda has its own file system and shell, so it
-- ignores the client's capabilities and never calls @fs/*@ or @terminal/*@.
initialized :: Value
initialized =
  object
    [ "protocolVersion" .= (1 :: Int)
    , "agentCapabilities"
        .= object
          [ "loadSession" .= True
          , "promptCapabilities" .= object ["image" .= False, "audio" .= False, "embeddedContext" .= False]
          , "mcpCapabilities" .= object ["http" .= False, "sse" .= False]
          ]
    , "agentInfo" .= object ["name" .= ("hilda" :: Text), "title" .= ("hilda" :: Text), "version" .= showVersion version]
    , "authMethods" .= ([] :: [Value])
    ]

-- | A session's working directory, resolved, or why it is unusable.
directory :: FilePath -> IO (Either Failure FilePath)
directory cwd
  | not (isAbsolute cwd) = pure (Left (invalidParams, "cwd must be an absolute path"))
  | otherwise =
      doesDirectoryExist cwd >>= \case
        False -> pure (Left (invalidParams, "no directory " <> T.pack cwd))
        -- Resolved as getcwd is, so hilda --resume in that directory matches it.
        True -> Right <$> canonicalizePath cwd

-- | A session with this history and cost, its system prompt built for @cwd@.
newSession :: Server -> Text -> FilePath -> FilePath -> [Message] -> Maybe Double -> IO Session
newSession srv sid path cwd msgs cost = do
  system <- srvSystem srv cwd
  Session sid cwd path system
    <$> newIORef msgs
    <*> newIORef mempty {usageCost = cost}
    <*> newIORef 0
    <*> newIORef Map.empty
    <*> newIORef Nothing
    <*> newIORef (length [() | Assistant {} <- msgs])

-- | Run a prompt in a new thread and answer request @i@ when it ends. The
-- turn runs in a thread of its own, which a cancel kills; its answer waits
-- until that thread has exited, so every update comes before it.
startPrompt :: Server -> Session -> Value -> Text -> IO ()
startPrompt srv s i text = do
  done <- newEmptyMVar
  answered <- newEmptyMVar
  key <- newUnique
  open <- newIORef Set.empty
  writeIORef (sesRunning s) (Just (void (tryPutMVar done Nothing)))
  -- Registered before the fork, so end of input waits for this answer.
  atomicModifyIORef' (srvAnswers srv) (\m -> (Map.insert key answered m, ()))
  -- forkFinally, not forkIO with finally: a kill that lands before the
  -- thread runs would skip the handler, and nothing would answer.
  void . flip forkFinally (const (putMVar answered () >> atomicModifyIORef' (srvAnswers srv) (\m -> (Map.delete key m, ())))) $ do
    exited <- newEmptyMVar
    worker <- forkFinally (runPrompt srv s open text) (\r -> void (tryPutMVar done (Just r)) >> putMVar exited ())
    result <- takeMVar done
    when (isNothing result) $ do
      killThread worker
      -- The killed turn's history is dropped, but its cost stays.
      void (try @IOException (persist s))
    takeMVar exited
    -- A killed turn leaves calls announced but unfinished.
    readIORef open >>= mapM_ (\tid -> update srv (sesId s) (object ["sessionUpdate" .= ("tool_call_update" :: Text), "toolCallId" .= tid, "status" .= ("failed" :: Text)]))
    writeIORef (sesRunning s) Nothing
    usage <- readIORef (sesSpent s)
    used <- readIORef (sesContext s)
    -- size is required, so an unknown window sends no update.
    tryReadMVar (srvLimits srv) >>= mapM_ (\window ->
      update srv (sesId s) . object $
        ["sessionUpdate" .= ("usage_update" :: Text), "used" .= used, "size" .= window]
          <> maybe [] (\c -> ["cost" .= object ["amount" .= c, "currency" .= currency]]) (usageCost usage)) . (>>= snd)
    case result of
      Nothing -> respond srv i (object ["stopReason" .= ("cancelled" :: Text)])
      Just (Left e) -> failWith srv i (internalError, T.pack (show e))
      Just (Right out) -> either (\e -> failWith srv i (internalError, e)) (\r -> respond srv i (object ["stopReason" .= r])) (stopReason (outStop out) (outFinish out))
  where
    -- OpenRouter credits are US dollars; other servers do not say.
    currency :: Text
    currency = if cfgProvider (srvCfg srv) == OpenRouter then "USD" else "credits"

-- | The ACP stop reason for a turn and how its last reply ended, or the
-- error a failed turn answers. A cost limit stops before the next model
-- request, so it reads as the request limit: no other reason says the
-- agent chose to stop.
stopReason :: Stop -> Finish -> Either Text Text
stopReason stop finish = case stop of
  Finished -> Right $ case finish of
    FinishStop -> "end_turn"
    FinishLength -> "max_tokens"
    FinishRefusal -> "refusal"
  TurnLimit -> Right "max_turn_requests"
  CostLimit -> Right "max_turn_requests"
  Failed e -> Left e

-- | One turn: stream it as updates, then save the history. @open@ holds
-- the ACP ids of calls announced and not yet finished.
runPrompt :: Server -> Session -> IORef (Set Text) -> Text -> IO Outcome
runPrompt srv s open text = withMVar (srvTurn srv) $ \_ -> do
  setCurrentDirectory (sesCwd s)
  history <- readIORef (sesHistory s)
  spent <- readIORef (sesSpent s)
  (budget, _) <- readMVar (srvLimits srv)
  -- Calls announced while asking permission, and whether they were allowed.
  asked <- newIORef Map.empty
  let say = update srv (sesId s)
      chunk kind t = say (object ["sessionUpdate" .= (kind :: Text), "content" .= textBlock t])
      complete sink r = do
        reply <- cfgComplete cfg (\d -> sink d >> case d of TextDelta t -> chunk "agent_message_chunk" t; ReasoningDelta t -> chunk "agent_thought_chunk" t) r
        reply <$ either (const (pure ())) (const (modifyIORef' (sesReplies s) (+ 1))) reply
      -- Model call ids repeat across replies; the reply number makes them unique.
      idOf c = (`acpId` callId c) <$> readIORef (sesReplies s)
      -- Recorded with the update, so a cancel finds every call announced.
      announce tid fields = mask_ (modifyIORef' open (Set.insert tid) >> say (object fields))
      -- Whether a call nobody was asked about runs: the mode, else a rule.
      runs c = case find ((== callName c) . toolName) (envTools env) of
        Nothing -> pure False
        Just tool -> case authorize (cfgMode cfg) (toolEffect tool) of
          Allow -> pure True
          Deny _ -> pure False
          Confirm -> (== Just True) . Map.lookup (callName c) <$> readIORef (sesRules s)
      onEv = \case
        CallStarted c -> do
          tid <- idOf c
          Map.lookup tid <$> readIORef asked >>= \case
            Nothing -> do
              -- A call denied without asking is pending until it fails.
              status <- (\ok -> if ok then "in_progress" else "pending") <$> runs c
              announce tid (("sessionUpdate" .= ("tool_call" :: Text)) : "status" .= (status :: Text) : callFields (sesCwd s) tid c)
            Just True -> say (object ["sessionUpdate" .= ("tool_call_update" :: Text), "toolCallId" .= tid, "status" .= ("in_progress" :: Text)])
            Just False -> pure ()
        CallFinished c r -> do
          tid <- idOf c
          mask_ . (modifyIORef' open (Set.delete tid) >>) . say . object $
            [ "sessionUpdate" .= ("tool_call_update" :: Text)
            , "toolCallId" .= tid
            , "status" .= either (const ("failed" :: Text)) (const "completed") r
            , "content" .= [object ["type" .= ("content" :: Text), "content" .= textBlock (either id id r)]]
            ]
        -- Already streamed as agent_message_chunk.
        Narration _ -> pure ()
        -- A sub-agent's calls are announced like the agent's own; their
        -- ids carry the sub-agent's label, so they cannot collide.
        Sub _ (Narration _) -> pure ()
        Sub _ ev -> onEv ev
        ev -> case renderEvent plain ev of
          Partial t -> unless (T.null t) (TIO.hPutStrLn stderr t)
          Full t -> TIO.hPutStrLn stderr t
      tools = builtinTools (resultLimitFor budget)
      env =
        attach (cfgFamily cfg) (sesSystem s) $ Env
          { envComplete = tally (sesSpent s) complete
          , envModel = cfgModel cfg
          , envTools = tools
          , envMode = cfgMode cfg
          , envMaxTurns = cfgMaxTurns cfg
          , envBudget = budget
          , envCostLimit = cfgCostLimit cfg
          , envSpent = fromMaybe 0 (usageCost spent)
          , envHooks = Hooks {onEvent = onEv, confirm = \c -> idOf c >>= \tid -> permit srv s announce asked tid c}
          }
  out <- runTurn env (System (sesSystem s) : history) text
  -- ACP leaves a refused prompt and all after it out of the next prompt.
  let refused = outStop out == Finished && outFinish out == FinishRefusal
  unless refused $ do
    writeIORef (sesHistory s) (drop 1 (outHistory out))
    -- A turn with no reply keeps the last known context size.
    when (outTurns out > 0) (writeIORef (sesContext s) (outContext out))
  when (outTurns out > 0) (logFailure "model not remembered" (cfgRemember cfg (cfgModel cfg)))
  logFailure "session not saved" (persist s)
  pure out
  where
    cfg = srvCfg srv
    logFailure what act =
      try @IOException act >>= either (\e -> hPutStrLn stderr ("hilda: " <> what <> ": " <> show e)) pure

-- | Save a session's history and cost to its file.
persist :: Session -> IO ()
persist s = do
  msgs <- readIORef (sesHistory s)
  spent <- readIORef (sesSpent s)
  saveIdSession (sesFile s) (sesCwd s) (usageCost spent) msgs

-- | Ask the client whether call @tid@ may run, announcing it first. A
-- cancelled request, an error, or an unknown option is a denial. An
-- "always" answer holds for that tool for the rest of the session.
permit :: Server -> Session -> (Text -> [Pair] -> IO ()) -> IORef (Map Text Bool) -> Text -> ToolCall -> IO Bool
permit srv s announce asked tid c =
  Map.lookup (callName c) <$> readIORef (sesRules s) >>= \case
    Just allowed -> pure allowed
    Nothing -> do
      let fields = "status" .= ("pending" :: Text) : callFields (sesCwd s) tid c
      announce tid (("sessionUpdate" .= ("tool_call" :: Text)) : fields)
      answer <- ask srv "session/request_permission" (object ["sessionId" .= sesId s, "toolCall" .= object fields, "options" .= options])
      let choice = either (const Nothing) Just (parseEither chosen answer)
          allowed = choice `elem` [Just "allow_once", Just "allow_always"]
      forM_ choice $ \o ->
        when (o `elem` ["allow_always", "reject_always"]) $
          modifyIORef' (sesRules s) (Map.insert (callName c) allowed)
      allowed <$ modifyIORef' asked (Map.insert tid allowed)
  where
    name = callName c
    options =
      [ option "allow_once" "Allow"
      , option "allow_always" ("Always allow " <> name <> " in this session")
      , option "reject_once" "Reject"
      , option "reject_always" ("Always reject " <> name <> " in this session")
      ]
    option :: Text -> Text -> Value
    option k n = object ["optionId" .= k, "name" .= n, "kind" .= k]
    chosen :: Value -> Parser Text
    chosen = withObject "response" $ \o -> do
      outcome <- o .: "result" >>= (.: "outcome")
      kind <- outcome .: "outcome"
      if kind == ("selected" :: Text) then outcome .: "optionId" else fail "not selected"

-- | The ACP id of a call in reply @n@ of a session.
acpId :: Int -> Text -> Text
acpId n i = T.pack (show n) <> "-" <> i

-- | The fields of a @tool_call@ with ACP id @tid@: title, kind, raw input
-- and the path it touches.
callFields :: FilePath -> Text -> ToolCall -> [Pair]
callFields cwd tid c =
  [ "toolCallId" .= tid
  , "title" .= (callName c <> ": " <> callSummary c)
  , "kind" .= toolKind (callName c)
  , "rawInput" .= fromMaybe (String (callArgs c)) args
  , "locations" .= [object ["path" .= (cwd </> T.unpack p)] | Just (Object o) <- [args], Just (String p) <- [KM.lookup "path" o]]
  ]
  where
    args = decodeStrict (encodeUtf8 (callArgs c)) :: Maybe Value

-- | The ACP tool kind of a built-in tool.
toolKind :: Text -> Text
toolKind = \case
  "read" -> "read"
  "write" -> "edit"
  "edit" -> "edit"
  "bash" -> "execute"
  _ -> "other"

-- | A text content block.
textBlock :: Text -> Value
textBlock t = object ["type" .= ("text" :: Text), "text" .= t]

-- | A prompt's content blocks as one user message, blocks separated by a
-- blank line. A resource link becomes its path or URI, which the model
-- can read with its tools. Other block types were not advertised. A
-- prompt with no text and no link is an error.
promptText :: [Value] -> Either Text Text
promptText blocks = do
  t <- T.intercalate "\n\n" <$> traverse (either (Left . T.pack) Right . parseEither block) blocks
  if T.all isSpace t then Left "Invalid params: the prompt is empty" else Right t
  where
    block = withObject "content block" $ \o ->
      o .: "type" >>= \case
        "text" -> o .: "text"
        "resource_link" -> ("Linked resource: " <>) . (\u -> fromMaybe u (T.stripPrefix "file://" u)) <$> o .: "uri"
        ty -> fail ("unsupported content block type " <> T.unpack ty)

-- | A saved history as @session/update@ payloads, for @session/load@.
-- Tool calls replay as completed, then failed if their result is an error.
-- Their ids are numbered by reply, as in a live turn.
replay :: FilePath -> [Message] -> [Value]
replay cwd = concat . snd . mapAccumL step 0
  where
    step n m = case m of
      Assistant {} -> (n + 1, updates (n + 1) m)
      _ -> (n, updates n m)
    updates :: Int -> Message -> [Value]
    updates n = \case
      System _ -> []
      User t -> [chunk "user_message_chunk" t]
      Assistant t calls _ ->
        [chunk "agent_message_chunk" x | Just x <- [t]]
          <> [object (("sessionUpdate" .= ("tool_call" :: Text)) : "status" .= ("completed" :: Text) : callFields cwd (acpId n (callId c)) c) | c <- calls]
      ToolResult i r ->
        [ object
            [ "sessionUpdate" .= ("tool_call_update" :: Text)
            , "toolCallId" .= acpId n i
            , "status" .= (if "error: " `T.isPrefixOf` r then "failed" else "completed" :: Text)
            , "content" .= [object ["type" .= ("content" :: Text), "content" .= textBlock r]]
            ]
        ]
    chunk :: Text -> Text -> Value
    chunk kind t = object ["sessionUpdate" .= kind, "content" .= textBlock t]
