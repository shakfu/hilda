-- | End-to-end headless sessions: runs the built @hilda@ against
-- 'FakeServer', with state in a temporary directory.
module HeadlessSpec (spec) where

import Data.Aeson (Value (..), decodeStrict, (.:))
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseMaybe, withObject)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import FakeServer
import Data.List (sort)
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import System.Directory (createDirectory, createDirectoryIfMissing, listDirectory, setModificationTime)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Hspec

field :: Text -> Value -> Maybe Value
field k = parseMaybe (withObject "result" (.: Key.fromText k))

-- | A plain JSON chat completion that answers @text@.
answer :: String -> Canned
answer text =
  json 200 . BS8.pack $
    "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"" <> text
      <> "\"}}],\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":2,\"cost\":0.001}}"

-- | Run hilda in @cwd@ with state under @state@, against @port@. Returns
-- the exit code, the last stdout line as JSON, and stderr.
hilda :: FilePath -> FilePath -> Int -> [String] -> IO (ExitCode, Maybe Value, String)
hilda = hildaWith []

-- | 'hilda' with extra environment variables.
hildaWith :: [(String, String)] -> FilePath -> FilePath -> Int -> [String] -> IO (ExitCode, Maybe Value, String)
hildaWith extra state cwd port args = do
  env <- filter ((`notElem` ["OPENROUTER_API_KEY", "OPENAI_API_KEY", "OPENAI_BASE_URL", "XDG_STATE_HOME", "HILDA_SESSION_DAYS"]) . fst) <$> getEnvironment
  let p = (proc "hilda" (["-P", "openai", "--base-url", "http://127.0.0.1:" <> show port <> "/v1", "-m", "x"] <> args))
        { cwd = Just cwd, env = Just (("XDG_STATE_HOME", state) : extra <> env) }
  (code, out, err) <- readCreateProcessWithExitCode p ""
  pure (code, decodeStrict . BS8.pack =<< lastMaybe (lines out), err)
  where
    lastMaybe xs = if null xs then Nothing else Just (last xs)

spec :: Spec
spec = around (withSystemTempDirectory "hilda") $ do
  it "saves a -p run under an id and resumes it with --resume" $ \dir -> do
    let state = dir </> "state"
    port <- freePort
    ((code1, r1, _), _) <- withServer port 0 [answer "first answer"] (hilda state dir (fromIntegral port) ["--json", "-p", "one"])
    code1 `shouldBe` ExitSuccess
    (field "result" =<< r1) `shouldBe` Just (String "first answer")
    sid <- case field "session_id" =<< r1 of
      Just (String s) -> pure s
      other -> fail ("no session_id: " <> show other)
    listDirectory (state </> "hilda" </> "sessions") `shouldReturn` [T.unpack sid <> ".json"]
    port2 <- freePort
    ((code2, r2, _), _) <- withServer port2 0 [answer "second answer"] (hilda state dir (fromIntegral port2) ["--resume", T.unpack sid, "--stream-json", "-p", "two"])
    code2 `shouldBe` ExitSuccess
    (field "result" =<< r2) `shouldBe` Just (String "second answer")
    (field "session_id" =<< r2) `shouldBe` Just (String sid)
    -- system, one, first answer, two, second answer
    (field "messages" =<< r2) `shouldSatisfy` \case
      Just (Array a) -> length a == 5
      _ -> False

  it "refuses a session saved in another directory" $ \dir -> do
    let state = dir </> "state"
        other = dir </> "other"
    createDirectory other
    port <- freePort
    ((_, r1, _), _) <- withServer port 0 [answer "hi"] (hilda state dir (fromIntegral port) ["--json", "-p", "one"])
    let sid = maybe "" (\case String s -> T.unpack s; _ -> "") (field "session_id" =<< r1)
    (code, r, err) <- hilda state other 1 ["--resume", sid, "--json", "-p", "two"]
    code `shouldBe` ExitFailure 1
    err `shouldContain` "saved in"
    (field "stop" =<< r) `shouldBe` Just (String "error")

  it "fails with an error result for an unknown or invalid id" $ \dir -> do
    let state = dir </> "state"
    (code, r, err) <- hilda state dir 1 ["--resume", "ffff", "--json", "-p", "x"]
    code `shouldBe` ExitFailure 1
    err `shouldContain` "no saved session"
    (field "error" =<< r) `shouldBe` Just (String "--resume ffff: no saved session with this id")
    (field "session_id" =<< r) `shouldBe` Just Null
    (code', r', _) <- hilda state dir 1 ["--resume", "../x", "--json", "-p", "x"]
    code' `shouldBe` ExitFailure 1
    (field "error" =<< r') `shouldSatisfy` maybe False (\case String e -> "invalid session id" `T.isInfixOf` e; _ -> False)

  it "delegates to a sub-agent with -r and reports the family's spending" $ \dir -> do
    let state = dir </> "state"
        delegate =
          json 200 . BS8.pack $
            "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,\"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\","
              <> "\"function\":{\"name\":\"agent\",\"arguments\":\"{\\\"task\\\":\\\"count\\\"}\"}}]}}],"
              <> "\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":2,\"cost\":0.001}}"
    port <- freePort
    -- One model at a time: the root, then the child, then the root again.
    ((code, r, _), _) <-
      withServer port 0 [delegate, answer "42", answer "it is 42"] $
        hilda state dir (fromIntegral port) ["-r", "--budget-tokens", "100000", "--no-session", "--json", "-p", "count"]
    code `shouldBe` ExitSuccess
    (field "result" =<< r) `shouldBe` Just (String "it is 42")
    (field "spent_tokens" =<< field "family" =<< r) `shouldBe` Just (Number 36)

  it "saves no transcript with --no-session and reports no session_id" $ \dir -> do
    let state = dir </> "state"
    port <- freePort
    ((code, r, _), _) <- withServer port 0 [answer "hi"] (hilda state dir (fromIntegral port) ["--no-session", "--json", "-p", "one"])
    code `shouldBe` ExitSuccess
    (field "result" =<< r) `shouldBe` Just (String "hi")
    (field "session_id" =<< r) `shouldBe` Just Null
    -- models.json, the last model used, is a setting, not a transcript.
    listDirectory (state </> "hilda") `shouldReturn` ["models.json"]

  it "deletes id sessions older than HILDA_SESSION_DAYS and keeps the rest" $ \dir -> do
    let state = dir </> "state"
        sessions = state </> "hilda" </> "sessions"
    createDirectoryIfMissing True sessions
    now <- getCurrentTime
    let saved name days = do
          writeFile (sessions </> name) "{}"
          setModificationTime (sessions </> name) (addUTCTime (negate (days * nominalDay)) now)
    saved "aaaa.json" 31
    saved "bbbb.json" 29
    saved "%2Fwork.json" 400
    saved "notes.json" 400
    (code0, _, _) <- hildaWith [("HILDA_SESSION_DAYS", "0")] state dir 1 ["--no-session", "--json", "-p", "x"]
    code0 `shouldBe` ExitFailure 1
    sort <$> listDirectory sessions `shouldReturn` ["%2Fwork.json", "aaaa.json", "bbbb.json", "notes.json"]
    (_, _, err) <- hildaWith [("HILDA_SESSION_DAYS", "soon")] state dir 1 ["--no-session", "--json", "-p", "x"]
    err `shouldContain` "no session was deleted"
    sort <$> listDirectory sessions `shouldReturn` ["%2Fwork.json", "aaaa.json", "bbbb.json", "notes.json"]
    _ <- hilda state dir 1 ["--no-session", "--json", "-p", "x"]
    sort <$> listDirectory sessions `shouldReturn` ["%2Fwork.json", "bbbb.json", "notes.json"]
