-- | ACP sessions end to end: runs the built @hilda --acp@ against
-- "Hilda.FakeServer" over pipes, with state in a temporary directory.
module AcpSpec (spec) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hilda.FakeServer
import Hilda.Acp
import Hilda.Agent (Stop (..))
import Hilda.Types (Finish (..))
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

-- | A running @hilda --acp@.
data Agent = Agent {agentIn :: Handle, agentOut :: Handle}

-- | The value at a path of object keys.
at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) v = parseMaybe (withObject "object" (.: Key.fromText k)) v >>= at ks

text :: [Text] -> Value -> Maybe Text
text ks v = at ks v >>= \case String s -> Just s; _ -> Nothing

-- | Run hilda in @cwd@ with state under @state@, against @port@, and close
-- its stdin afterwards. It must then exit within 5 s.
withAgent :: FilePath -> FilePath -> Int -> [String] -> (Agent -> IO a) -> IO a
withAgent = withProvider "openai" []

-- | 'withAgent' with this provider and extra environment.
withProvider :: String -> [(String, String)] -> FilePath -> FilePath -> Int -> [String] -> (Agent -> IO a) -> IO a
withProvider provider extra state cwd port args k = do
  env <- filter ((`notElem` ["OPENROUTER_API_KEY", "OPENAI_API_KEY", "OPENAI_BASE_URL", "XDG_STATE_HOME"]) . fst) <$> getEnvironment
  errors <- openFile (state <> ".stderr") AppendMode
  let p = (proc "hilda" (["--acp", "-P", provider, "--base-url", "http://127.0.0.1:" <> show port <> "/v1", "-m", "x"] <> args))
        { cwd = Just cwd, env = Just (("XDG_STATE_HOME", state) : extra <> env)
        , std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle errors
        }
  (Just i, Just o, _, ph) <- createProcess p
  r <- k (Agent i o) `finally` hClose i
  timeout 5000000 (waitForProcess ph) `shouldReturn` Just ExitSuccess
  pure r

-- | Send one message.
put :: Agent -> Value -> IO ()
put (Agent i _) v = BL.hPutStrLn i (encode v) >> hFlush i

-- | Send request @n@ and read until its response, answering each request
-- from hilda with @answer@. Returns the response and the notifications before it.
exchange :: Agent -> Int -> Text -> Value -> (Value -> Value) -> IO (Value, [Value])
exchange a n method params reply = do
  put a (object ["jsonrpc" .= ("2.0" :: Text), "id" .= n, "method" .= method, "params" .= params])
  go []
  where
    go seen =
      timeout 10000000 (BS8.hGetLine (agentOut a)) >>= \case
        Nothing -> fail ("no response to " <> T.unpack method)
        Just line -> case decodeStrict line of
          Nothing -> fail ("stdout carried a line that is not JSON: " <> BS8.unpack line)
          Just v
            | at ["id"] v == Just (toJSON n), at ["method"] v == Nothing -> pure (v, reverse seen)
            | Just i <- at ["id"] v -> do
                put a (object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "result" .= reply v])
                go (v : seen)
            | otherwise -> go (v : seen)

-- | 'exchange' for a request during which hilda asks nothing.
call :: Agent -> Int -> Text -> Value -> IO (Value, [Value])
call a n method params = exchange a n method params (const Null)

initialize :: Agent -> IO Value
initialize a = fst <$> call a 0 "initialize" (object ["protocolVersion" .= (1 :: Int)])

newSession :: Agent -> FilePath -> IO Text
newSession a dir =
  call a 1 "session/new" (object ["cwd" .= dir, "mcpServers" .= ([] :: [Value])]) >>= \(r, _) ->
    maybe (fail ("no sessionId: " <> show r)) pure (text ["result", "sessionId"] r)

prompt :: Text -> Text -> Value
prompt sid t = object ["sessionId" .= sid, "prompt" .= [object ["type" .= ("text" :: Text), "text" .= t]]]

-- | The sessionUpdate kinds among notifications.
kinds :: [Value] -> [Text]
kinds = mapMaybe (text ["params", "update", "sessionUpdate"])

answer :: String -> Canned
answer t = answerWith t "stop"

-- | A reply with this text and finish_reason.
answerWith :: String -> String -> Canned
answerWith t reason =
  json 200 . BS8.pack $
    "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"" <> t
      <> "\"},\"finish_reason\":\"" <> reason <> "\"}],\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":2,\"cost\":0.001}}"

-- | A reply that writes @out.txt@.
writeCall :: Canned
writeCall =
  json 200
    "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,\"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\",\"function\":{\"name\":\"write\",\"arguments\":\"{\\\"path\\\":\\\"out.txt\\\",\\\"content\\\":\\\"x\\\"}\"}}]}}]}"

-- | A reply that runs @command@ in bash, with call id c1.
bashCall :: String -> Canned
bashCall command =
  json 200 . BS8.pack $
    "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,\"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\",\"function\":{\"name\":\"bash\",\"arguments\":\"{\\\"command\\\":\\\"" <> command <> "\\\"}\"}}]}}]}"

-- | Send prompt request @n@ without waiting.
send :: Agent -> Int -> Value -> IO ()
send a n params = put a (object ["jsonrpc" .= ("2.0" :: Text), "id" .= n, "method" .= ("session/prompt" :: Text), "params" .= params])

-- | Read messages until one satisfies @done@; return all read, in order.
readUntil :: Agent -> (Value -> Bool) -> IO [Value]
readUntil a done = go []
  where
    go seen =
      timeout 10000000 (BS8.hGetLine (agentOut a)) >>= \case
        Nothing -> fail "timed out reading hilda's output"
        Just line -> case decodeStrict line of
          Nothing -> fail ("not JSON: " <> BS8.unpack line)
          Just v
            | done v -> pure (reverse (v : seen))
            | otherwise -> go (v : seen)

-- | A @session/cancel@ for @sid@.
sendCancel :: Agent -> Text -> IO ()
sendCancel a sid = put a (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("session/cancel" :: Text), "params" .= object ["sessionId" .= sid]])

-- | The response to request @n@.
isResponse :: Int -> Value -> Bool
isResponse n v = at ["id"] v == Just (toJSON n) && at ["method"] v == Nothing

-- | The (toolCallId, status) of each tool call update.
statuses :: [Value] -> [(Text, Text)]
statuses ups = [(i, st) | u <- ups, Just i <- [text ["params", "update", "toolCallId"] u], Just st <- [text ["params", "update", "status"] u]]

-- | The saved session file of @sid@ under @state@.
sessionJson :: FilePath -> Text -> IO (Maybe Value)
sessionJson state sid = decodeStrict <$> BS8.readFile (state </> "hilda" </> "sessions" </> T.unpack sid <> ".json")

-- | A permission answer selecting @option@.
select :: Text -> Value -> Value
select option _ = object ["outcome" .= object ["outcome" .= ("selected" :: Text), "optionId" .= option]]

spec :: Spec
spec = do
  describe "pure parts" $ do
    it "maps stops to ACP stop reasons, failures to an error" $ do
      map (`stopReason` FinishStop) [Finished, TurnLimit, CostLimit] `shouldBe` map Right ["end_turn", "max_turn_requests", "max_turn_requests"]
      map (stopReason Finished) [FinishLength, FinishRefusal] `shouldBe` map Right ["max_tokens", "refusal"]
      stopReason (Failed "boom") FinishStop `shouldBe` Left "boom"
    it "joins text blocks and names resource links" $ do
      let block ty k v = object ["type" .= (ty :: Text), k .= (v :: Text)]
      promptText [block "text" "text" "look", block "resource_link" "uri" "file:///a/b.hs"] `shouldBe` Right "look\n\nLinked resource: /a/b.hs"
      promptText [block "image" "data" "x"] `shouldSatisfy` either (const True) (const False)
    it "maps tools to kinds" $
      map toolKind ["read", "write", "edit", "bash", "web"] `shouldBe` ["read", "edit", "edit", "execute", "other"]

  around (withSystemTempDirectory "hilda") $ do
    it "refuses requests before initialize, unknown methods and bad JSON" $ \dir0 -> do
      dir <- canonicalizePath dir0
      withAgent (dir </> "state") dir 1 [] $ \a -> do
        (r, _) <- call a 5 "session/new" (object ["cwd" .= dir])
        at ["error", "code"] r `shouldBe` Just (Number (-32600))
        i <- initialize a
        at ["result", "protocolVersion"] i `shouldBe` Just (Number 1)
        at ["result", "agentCapabilities", "loadSession"] i `shouldBe` Just (Bool True)
        (u, _) <- call a 6 "no/such" (object [])
        at ["error", "code"] u `shouldBe` Just (Number (-32601))
        (rel, _) <- call a 7 "session/new" (object ["cwd" .= ("relative" :: Text)])
        at ["error", "code"] rel `shouldBe` Just (Number (-32602))
        BS8.hPutStrLn (agentIn a) "not json" >> hFlush (agentIn a)
        v <- decodeStrict <$> BS8.hGetLine (agentOut a)
        (at ["error", "code"] =<< v) `shouldBe` Just (Number (-32700))

    it "streams a reply, saves it, and replays it on session/load in a new process" $ \dir0 -> do
      dir <- canonicalizePath dir0
      let state = dir </> "state"
      port <- freePort
      (sid, _) <- withServer port 0 [answer "first answer"] . withAgent state dir (fromIntegral port) ["--context-window", "200000"] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        (r, ups) <- call a 2 "session/prompt" (prompt sid "one")
        text ["result", "stopReason"] r `shouldBe` Just "end_turn"
        kinds ups `shouldBe` ["agent_message_chunk", "usage_update"]
        mapMaybe (text ["params", "update", "content", "text"]) ups `shouldBe` ["first answer"]
        at ["params", "update", "cost", "amount"] (last ups) `shouldBe` Just (Number 0.001)
        at ["params", "update", "size"] (last ups) `shouldBe` Just (Number 200000)
        pure sid
      withAgent state dir 1 [] $ \a -> do
        _ <- initialize a
        (r, ups) <- call a 2 "session/load" (object ["sessionId" .= sid, "cwd" .= dir, "mcpServers" .= ([] :: [Value])])
        at ["result"] r `shouldBe` Just (object [])
        kinds ups `shouldBe` ["user_message_chunk", "agent_message_chunk"]
        mapMaybe (text ["params", "update", "content", "text"]) ups `shouldBe` ["one", "first answer"]
        (missing, _) <- call a 3 "session/load" (object ["sessionId" .= ("ffff" :: Text), "cwd" .= dir])
        at ["error", "code"] missing `shouldBe` Just (Number (-32002))
      -- The same id resumes with -p.
      port2 <- freePort
      ((code, out, _), _) <- withServer port2 0 [answer "second"] $ do
        env <- filter ((`notElem` ["OPENAI_API_KEY", "OPENROUTER_API_KEY", "XDG_STATE_HOME"]) . fst) <$> getEnvironment
        readCreateProcessWithExitCode
          (proc "hilda" ["-P", "openai", "--base-url", "http://127.0.0.1:" <> show port2 <> "/v1", "-m", "x", "--resume", T.unpack sid, "--json", "-p", "two"])
            { cwd = Just dir, env = Just (("XDG_STATE_HOME", state) : env) }
          ""
      code `shouldBe` ExitSuccess
      (at ["messages"] =<< decodeStrict (BS8.pack (last (lines out)))) `shouldSatisfy` \case
        Just (Array m) -> length m == 5
        _ -> False

    it "sends no usage_update while the context window is unknown" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [answer "hi"] . withAgent (dir </> "state") dir (fromIntegral port) [] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        (_, ups) <- call a 2 "session/prompt" (prompt sid "one")
        kinds ups `shouldBe` ["agent_message_chunk"]
      pure ()

    it "keeps the session's cost across session/load in a new process" $ \dir0 -> do
      dir <- canonicalizePath dir0
      let state = dir </> "state"
          cost ups = at ["params", "update", "cost", "amount"] (last ups)
      port <- freePort
      (sid, _) <- withServer port 0 [answer "a", answer "b"] . withAgent state dir (fromIntegral port) ["--context-window", "1000"] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        _ <- call a 2 "session/prompt" (prompt sid "one")
        (_, ups) <- call a 3 "session/prompt" (prompt sid "two")
        cost ups `shouldBe` Just (Number 0.002)
        pure sid
      port2 <- freePort
      _ <- withServer port2 0 [answer "c"] . withAgent state dir (fromIntegral port2) ["--context-window", "1000"] $ \a -> do
        _ <- initialize a
        _ <- call a 2 "session/load" (object ["sessionId" .= sid, "cwd" .= dir, "mcpServers" .= ([] :: [Value])])
        (_, ups) <- call a 3 "session/prompt" (prompt sid "three")
        cost ups `shouldBe` Just (Number 0.003)
      pure ()

    it "answers session/new while the context window lookup stalls" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [Stall "application/json" "", Stall "application/json" ""] . withProvider "openrouter" [("OPENROUTER_API_KEY", "k")] (dir </> "state") dir (fromIntegral port) [] $ \a -> do
        _ <- initialize a
        -- The lookup gives up after 2 s.
        timeout 1000000 (newSession a dir) >>= (`shouldSatisfy` (/= Nothing))
      pure ()

    it "loads a session that was never prompted" $ \dir0 -> do
      dir <- canonicalizePath dir0
      let state = dir </> "state"
      sid <- withAgent state dir 1 [] $ \a -> initialize a >> newSession a dir
      withAgent state dir 1 [] $ \a -> do
        _ <- initialize a
        (r, ups) <- call a 2 "session/load" (object ["sessionId" .= sid, "cwd" .= dir, "mcpServers" .= ([] :: [Value])])
        at ["result"] r `shouldBe` Just (object [])
        ups `shouldBe` []

    it "answers max_tokens for a truncated reply and refusal for a filtered one" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [answerWith "cut" "length", answerWith "" "content_filter"] . withAgent (dir </> "state") dir (fromIntegral port) [] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        (r1, _) <- call a 2 "session/prompt" (prompt sid "one")
        text ["result", "stopReason"] r1 `shouldBe` Just "max_tokens"
        (r2, _) <- call a 3 "session/prompt" (prompt sid "two")
        text ["result", "stopReason"] r2 `shouldBe` Just "refusal"
      pure ()

    it "reads a relative --system-file against the start directory" $ \dir0 -> do
      dir <- canonicalizePath dir0
      let sub = dir </> "sub"
      createDirectory sub
      writeFile (dir </> "sys.txt") "be brief"
      port <- freePort
      _ <- withServer port 0 [answer "hi"] . withAgent (dir </> "state") dir (fromIntegral port) ["--system-file", "sys.txt"] $ \a -> do
        _ <- initialize a
        sid <- newSession a sub
        _ <- call a 2 "session/prompt" (prompt sid "one")
        -- The prompt ran in sub; a new session still finds sys.txt.
        (r, _) <- call a 3 "session/new" (object ["cwd" .= dir, "mcpServers" .= ([] :: [Value])])
        text ["result", "sessionId"] r `shouldSatisfy` (/= Nothing)
      pure ()

    it "answers a prompt whose input ends while it finishes" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [answer "hi"] . withAgent (dir </> "state") dir (fromIntegral port) [] $ \a@(Agent i o) -> do
        _ <- initialize a
        sid <- newSession a dir
        put a (object ["jsonrpc" .= ("2.0" :: Text), "id" .= (2 :: Int), "method" .= ("session/prompt" :: Text), "params" .= prompt sid "one"])
        -- The chunk comes just before the answer; close input then.
        _ <- BS8.hGetLine o
        hClose i
        let response = decodeStrict <$> BS8.hGetLine o >>= \v -> if (at ["id"] =<< v) == Just (Number 2) then pure v else response
        r <- timeout 5000000 response
        (text ["result", "stopReason"] =<< (r >>= id)) `shouldSatisfy` (`elem` [Just "end_turn", Just "cancelled"])
      pure ()

    it "in ask mode, runs a call only when the client allows it" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [writeCall, answer "no", writeCall, answer "yes"] . withAgent (dir </> "state") dir (fromIntegral port) ["-M", "ask"] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        -- A cancelled request is a denial.
        (r1, ups1) <- exchange a 2 "session/prompt" (prompt sid "write it") (const (object ["outcome" .= object ["outcome" .= ("cancelled" :: Text)]]))
        text ["result", "stopReason"] r1 `shouldBe` Just "end_turn"
        mapMaybe (text ["method"]) ups1 `shouldContain` ["session/request_permission"]
        mapMaybe (text ["params", "update", "status"]) ups1 `shouldBe` ["pending", "failed"]
        doesFileExist (dir </> "out.txt") `shouldReturn` False
        (r2, ups2) <- exchange a 3 "session/prompt" (prompt sid "write it") (select "allow_once")
        text ["result", "stopReason"] r2 `shouldBe` Just "end_turn"
        mapMaybe (text ["params", "update", "status"]) ups2 `shouldBe` ["pending", "in_progress", "completed"]
        mapMaybe (text ["params", "toolCall", "kind"]) ups2 `shouldBe` ["edit"]
        doesFileExist (dir </> "out.txt") `shouldReturn` True
      pure ()

    it "answers every prompt whose cancel comes in the same write" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 (replicate 40 (answer "hi")) . withAgent (dir </> "state") dir (fromIntegral port) [] $ \a@(Agent i o) -> do
        _ <- initialize a
        sid <- newSession a dir
        -- A cancel read before the turn's thread runs once left it unanswered.
        forM_ [2 .. 31 :: Int] $ \n -> do
          let request = object ["jsonrpc" .= ("2.0" :: Text), "id" .= n, "method" .= ("session/prompt" :: Text), "params" .= prompt sid "hi"]
              cancel = object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("session/cancel" :: Text), "params" .= object ["sessionId" .= sid]]
          BL.hPut i (encode request <> "\n" <> encode cancel <> "\n") >> hFlush i
          let response = decodeStrict <$> BS8.hGetLine o >>= \v -> if (at ["id"] =<< v) == Just (toJSON n) then pure v else response
          r <- timeout 5000000 response
          (text ["result", "stopReason"] =<< (r >>= id)) `shouldSatisfy` (`elem` [Just "end_turn", Just "cancelled"])
      pure ()

    it "answers a cancelled prompt with stopReason cancelled" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [Stall "text/event-stream" "data: {\"choices\":[{\"delta\":{\"content\":\"Hel\"}}]}\n\n"] . withAgent (dir </> "state") dir (fromIntegral port) [] $ \a@(Agent _ o) -> do
        _ <- initialize a
        sid <- newSession a dir
        put a (object ["jsonrpc" .= ("2.0" :: Text), "id" .= (2 :: Int), "method" .= ("session/prompt" :: Text), "params" .= prompt sid "slow"])
        -- The first chunk shows the model call is running.
        first' <- decodeStrict <$> BS8.hGetLine o
        (text ["params", "update", "content", "text"] =<< first') `shouldBe` Just "Hel"
        (r, _) <- call a 3 "session/prompt" (prompt sid "again")
        at ["error", "code"] r `shouldBe` Just (Number (-32600))
        put a (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("session/cancel" :: Text), "params" .= object ["sessionId" .= sid]])
        let response = decodeStrict <$> BS8.hGetLine o >>= \v -> if (at ["id"] =<< v) == Just (Number 2) then pure v else response
        r2 <- timeout 5000000 response
        (text ["result", "stopReason"] =<< (r2 >>= id)) `shouldBe` Just "cancelled"
      pure ()

    it "answers prompts when the context window cannot be cached" $ \dir0 -> do
      dir <- canonicalizePath dir0
      let state = dir </> "state"
          endpoints = "{\"data\":{\"endpoints\":[{\"context_length\":64000}]}}"
      createDirectoryIfMissing True (state </> "hilda" </> "contexts.json")
      port <- freePort
      (_, served) <- withServer port 0 [json 200 endpoints, answer "hi"] . withProvider "openrouter" [("OPENROUTER_API_KEY", "k")] state dir (fromIntegral port) [] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        (r, ups) <- call a 2 "session/prompt" (prompt sid "one")
        text ["result", "stopReason"] r `shouldBe` Just "end_turn"
        at ["params", "update", "size"] (last ups) `shouldBe` Just (Number 64000)
      -- One window lookup, one model call.
      served `shouldBe` 2

    it "answers and saves a turn when the model cannot be remembered" $ \dir0 -> do
      dir <- canonicalizePath dir0
      let state = dir </> "state"
      createDirectoryIfMissing True (state </> "hilda" </> "models.json")
      port <- freePort
      _ <- withServer port 0 [answer "hi"] . withAgent state dir (fromIntegral port) [] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        (r, _) <- call a 2 "session/prompt" (prompt sid "one")
        text ["result", "stopReason"] r `shouldBe` Just "end_turn"
        saved <- sessionJson state sid
        (at ["messages"] =<< saved) `shouldSatisfy` \case
          Just (Array m) -> length m == 2
          _ -> False
      pure ()

    it "leaves a refused prompt out of the history but keeps its cost" $ \dir0 -> do
      dir <- canonicalizePath dir0
      let state = dir </> "state"
      port <- freePort
      _ <- withServer port 0 [answer "a", answerWith "no" "content_filter", answer "b"] . withAgent state dir (fromIntegral port) ["--context-window", "1000"] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        _ <- call a 2 "session/prompt" (prompt sid "one")
        (r, _) <- call a 3 "session/prompt" (prompt sid "two")
        text ["result", "stopReason"] r `shouldBe` Just "refusal"
        saved <- sessionJson state sid
        (at ["messages"] =<< saved) `shouldSatisfy` \case
          Just (Array m) -> length m == 2
          _ -> False
        (at ["cost"] =<< saved) `shouldBe` Just (Number 0.002)
        (_, ups) <- call a 4 "session/prompt" (prompt sid "three")
        at ["params", "update", "cost", "amount"] (last ups) `shouldBe` Just (Number 0.003)
      pure ()

    it "fails a running call and a call awaiting permission on cancel" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [bashCall "sleep 5", bashCall "true"] . withAgent (dir </> "state") dir (fromIntegral port) ["-M", "ask"] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        -- Allowed, then cancelled while it runs.
        send a 2 (prompt sid "run")
        ask1 <- readUntil a (\v -> text ["method"] v == Just "session/request_permission")
        Just i <- pure (at ["id"] (last ask1))
        put a (object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "result" .= select "allow_once" Null])
        _ <- readUntil a (\v -> text ["params", "update", "status"] v == Just "in_progress")
        sendCancel a sid
        ups1 <- readUntil a (isResponse 2)
        text ["result", "stopReason"] (last ups1) `shouldBe` Just "cancelled"
        map snd (statuses ups1) `shouldBe` ["failed"]
        -- Cancelled while the client has not answered.
        send a 3 (prompt sid "run")
        ask2 <- readUntil a (\v -> text ["method"] v == Just "session/request_permission")
        sendCancel a sid
        ups2 <- readUntil a (isResponse 3)
        map snd (statuses (ask2 <> ups2)) `shouldBe` ["pending", "failed"]
      pure ()

    it "gives tool calls ids unique in the session, the same on session/load" $ \dir0 -> do
      dir <- canonicalizePath dir0
      let state = dir </> "state"
          calls ups = [i | (i, st) <- statuses ups, st == "in_progress"]
      port <- freePort
      (live, _) <- withServer port 0 [writeCall, answer "a", writeCall, answer "b"] . withAgent state dir (fromIntegral port) ["-M", "yolo"] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        (_, ups1) <- call a 2 "session/prompt" (prompt sid "one")
        (_, ups2) <- call a 3 "session/prompt" (prompt sid "two")
        (x, y) <- case calls (ups1 <> ups2) of
          [x, y] -> pure (x, y)
          other -> fail ("expected two calls: " <> show other)
        x `shouldNotBe` y
        -- Each update names its call's id.
        map fst (statuses ups1) `shouldBe` [x, x]
        pure (sid, [x, y])
      withAgent state dir 1 [] $ \a -> do
        _ <- initialize a
        (_, ups) <- call a 2 "session/load" (object ["sessionId" .= fst live, "cwd" .= dir, "mcpServers" .= ([] :: [Value])])
        [i | (i, "completed") <- statuses ups] `shouldBe` concatMap (replicate 2) (snd live)

    it "keeps the context size after a turn whose model call failed" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [answer "a", json 400 "{}"] . withAgent (dir </> "state") dir (fromIntegral port) ["--context-window", "1000"] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        _ <- call a 2 "session/prompt" (prompt sid "one")
        (r, ups) <- call a 3 "session/prompt" (prompt sid "two")
        at ["error", "code"] r `shouldBe` Just (Number (-32603))
        at ["params", "update", "used"] (last ups) `shouldBe` Just (Number 10)
      pure ()

    it "asks permission by default" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [writeCall, answer "no"] . withAgent (dir </> "state") dir (fromIntegral port) [] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        (_, ups) <- exchange a 2 "session/prompt" (prompt sid "write it") (select "reject_once")
        mapMaybe (text ["method"]) ups `shouldContain` ["session/request_permission"]
        doesFileExist (dir </> "out.txt") `shouldReturn` False
      pure ()

    it "refuses an empty prompt" $ \dir0 -> do
      dir <- canonicalizePath dir0
      withAgent (dir </> "state") dir 1 [] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        forM_ [[], [object ["type" .= ("text" :: Text), "text" .= ("" :: Text)]]] $ \blocks -> do
          (r, _) <- call a 2 "session/prompt" (object ["sessionId" .= sid, "prompt" .= blocks])
          at ["error", "code"] r `shouldBe` Just (Number (-32602))

    it "never reports a call denied without asking as in progress" $ \dir0 -> do
      dir <- canonicalizePath dir0
      port <- freePort
      _ <- withServer port 0 [writeCall, answer "no", writeCall, answer "no"] . withAgent (dir </> "state") dir (fromIntegral port) ["-M", "ask"] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        _ <- exchange a 2 "session/prompt" (prompt sid "write it") (select "reject_always")
        -- The rule answers; nothing is asked.
        (_, ups) <- call a 3 "session/prompt" (prompt sid "write it")
        mapMaybe (text ["method"]) ups `shouldNotContain` ["session/request_permission"]
        map snd (statuses ups) `shouldBe` ["pending", "failed"]
      port2 <- freePort
      _ <- withServer port2 0 [writeCall, answer "no"] . withAgent (dir </> "state") dir (fromIntegral port2) ["-M", "read-only"] $ \a -> do
        _ <- initialize a
        sid <- newSession a dir
        (_, ups) <- call a 2 "session/prompt" (prompt sid "write it")
        map snd (statuses ups) `shouldBe` ["pending", "failed"]
      doesFileExist (dir </> "out.txt") `shouldReturn` False

