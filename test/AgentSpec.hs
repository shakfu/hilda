module AgentSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Types (parseMaybe, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hilda.Agent
import Hilda.Policy
import Hilda.Tools (builtinTools)
import Hilda.Types
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | A backend that replays canned replies and records every request.
scripted :: [Reply] -> IO (Complete, IO [Request])
scripted replies = do
  queue <- newIORef replies
  seen <- newIORef []
  let complete _ r = do
        modifyIORef seen (r :)
        atomicModifyIORef' queue $ \case
          [] -> ([], Left "script exhausted")
          (x : xs) -> (xs, Right x)
  pure (complete, reverse <$> readIORef seen)

mkEnv :: Complete -> Mode -> Bool -> Env IO
mkEnv complete mode answer =
  Env
    { envComplete = complete
    , envModel = "test-model"
    , envTools = builtinTools
    , envMode = mode
    , envMaxTurns = 10
    , envBudget = 1000000
    , envCostLimit = Nothing
    , envSpent = 0
    , envHooks = Hooks {onEvent = const (pure ()), confirm = const (pure answer)}
    }

call :: Text -> Text -> Value -> ToolCall
call i name args = ToolCall i name (T.pack (BL.unpack (encode args)))

reply :: Maybe Text -> [ToolCall] -> Reply
reply t cs = Reply t cs (Usage 10 5 0 (Just 0.5))

-- | Tool results in the order they were appended.
toolResults :: [Message] -> [(Text, Text)]
toolResults hist = [(i, c) | ToolResult i c <- hist]

toolNames :: Request -> [Text]
toolNames = mapMaybe (parseMaybe (withObject "spec" (\o -> o .: "function" >>= withObject "fn" (.: "name")))) . reqTools

writeCall :: FilePath -> ToolCall
writeCall path = call "c1" "write" (object ["path" .= path, "content" .= ("hi" :: Text)])

spec :: Spec
spec = do
  it "returns the text of a reply without tool calls" $ do
    (complete, _) <- scripted [reply (Just "done") []]
    out <- runTurn (mkEnv complete Yolo True) [System "sys"] "hello"
    outStop out `shouldBe` Finished
    outText out `shouldBe` "done"
    outTurns out `shouldBe` 1
    outHistory out `shouldBe` [System "sys", User "hello", Assistant (Just "done") []]

  it "runs tool calls and feeds results back" $ withSystemTempDirectory "hilda" $ \dir -> do
    let path = dir </> "a.txt"
    (complete, requests) <- scripted [reply Nothing [writeCall path], reply (Just "ok") []]
    out <- runTurn (mkEnv complete Yolo True) [] "write it"
    readFile path `shouldReturn` "hi"
    outStop out `shouldBe` Finished
    outUsage out `shouldBe` Usage 20 10 0 (Just 1.0)
    reqs <- requests
    length reqs `shouldBe` 2
    map fst (toolResults (reqMessages (reqs !! 1))) `shouldBe` ["c1"]

  it "hides and refuses mutating tools in read-only mode" $ withSystemTempDirectory "hilda" $ \dir -> do
    let path = dir </> "a.txt"
    (complete, requests) <- scripted [reply Nothing [writeCall path], reply (Just "ok") []]
    out <- runTurn (mkEnv complete ReadOnly True) [] "write it"
    doesFileExist path `shouldReturn` False
    map snd (toolResults (outHistory out)) `shouldSatisfy` all ("error: refused" `T.isPrefixOf`)
    requests >>= \case
      (first : _) -> toolNames first `shouldBe` ["read"]
      [] -> expectationFailure "no request sent"

  it "does not run a tool the user declines in ask mode" $ withSystemTempDirectory "hilda" $ \dir -> do
    let path = dir </> "a.txt"
    (complete, _) <- scripted [reply Nothing [writeCall path], reply (Just "ok") []]
    out <- runTurn (mkEnv complete Ask False) [] "write it"
    doesFileExist path `shouldReturn` False
    map snd (toolResults (outHistory out)) `shouldBe` ["error: the user declined this tool call"]

  it "asks for confirmation before announcing the call" $ withSystemTempDirectory "hilda" $ \dir -> do
    log' <- newIORef []
    (complete, _) <- scripted [reply Nothing [writeCall (dir </> "a.txt")], reply (Just "ok") []]
    let record x = modifyIORef log' (x :)
        hooks =
          Hooks
            { onEvent = \case
                CallStarted _ -> record "started"
                CallFinished _ _ -> record "finished"
                Narration _ -> record "narration"
                ContextTrimmed {} -> record "trimmed"
                CostUnknown -> record "cost unknown"
            , confirm = \_ -> record "confirm" >> pure True
            }
    _ <- runTurn (mkEnv complete Ask True) {envHooks = hooks} [] "write it"
    reverse <$> readIORef log' `shouldReturn` ["confirm", "started", "finished" :: Text]

  it "runs a tool the user approves in ask mode" $ withSystemTempDirectory "hilda" $ \dir -> do
    let path = dir </> "a.txt"
    (complete, _) <- scripted [reply Nothing [writeCall path], reply (Just "ok") []]
    _ <- runTurn (mkEnv complete Ask True) [] "write it"
    doesFileExist path `shouldReturn` True

  it "reports unknown tools, bad arguments and IO errors as tool results" $ do
    let calls =
          [ ToolCall "c1" "nope" "{}"
          , ToolCall "c2" "read" "{not json"
          , call "c3" "read" (object ["path" .= ("/nonexistent/hilda" :: Text)])
          ]
    (complete, _) <- scripted [reply Nothing calls, reply (Just "ok") []]
    out <- runTurn (mkEnv complete Yolo True) [] "go"
    outStop out `shouldBe` Finished
    case map snd (toolResults (outHistory out)) of
      [a, b, c] -> do
        a `shouldBe` "error: unknown tool: nope"
        b `shouldSatisfy` T.isPrefixOf "error: invalid arguments"
        c `shouldSatisfy` T.isInfixOf "does not exist"
      other -> expectationFailure (show other)

  it "stops at the turn limit" $ do
    let loopReply = reply Nothing [call "c" "read" (object ["path" .= ("/nonexistent" :: Text)])]
    (complete, _) <- scripted (replicate 5 loopReply)
    out <- runTurn (mkEnv complete Yolo True) {envMaxTurns = 2} [] "go"
    outStop out `shouldBe` TurnLimit
    outTurns out `shouldBe` 2

  it "elides old tool results over budget and reports it" $ do
    events <- newIORef []
    (complete, requests) <- scripted [reply (Just "ok") []]
    let old = [User "a", Assistant Nothing [ToolCall "c1" "read" "{}"], ToolResult "c1" (T.replicate 8000 "x"), Assistant (Just "done") []]
        env = (mkEnv complete Yolo True) {envBudget = 100}
        hooks = (envHooks env) {onEvent = \case
          ContextTrimmed n _ -> modifyIORef events (n :)
          _ -> pure ()}
    out <- runTurn env {envHooks = hooks} old "next"
    readIORef events `shouldReturn` [1]
    requests >>= \case
      [r] -> map snd (toolResults (reqMessages r)) `shouldSatisfy` all ("[elided" `T.isPrefixOf`)
      rs -> expectationFailure (show (length rs))
    outContext out `shouldBe` 10

  it "stops before the call that would pass the cost limit" $ do
    let loopReply = reply Nothing [call "c" "read" (object ["path" .= ("/nonexistent" :: Text)])]
    (complete, _) <- scripted (replicate 5 loopReply)
    out <- runTurn (mkEnv complete Yolo True) {envCostLimit = Just 0.9} [] "go"
    outStop out `shouldBe` CostLimit
    outTurns out `shouldBe` 2
    -- Every tool call has its result, so the history can continue.
    last (outHistory out) `shouldSatisfy` \case
      ToolResult {} -> True
      _ -> False

  it "counts cost spent before the turn" $ do
    (complete, requests) <- scripted [reply (Just "ok") []]
    out <- runTurn (mkEnv complete Yolo True) {envCostLimit = Just 1, envSpent = 1} [] "go"
    outStop out `shouldBe` CostLimit
    requests `shouldReturn` []

  it "warns when a cost limit is set but no cost is reported" $ do
    warned <- newIORef False
    (complete, _) <- scripted [Reply (Just "ok") [] mempty]
    let env = (mkEnv complete Yolo True) {envCostLimit = Just 1}
        hooks = (envHooks env) {onEvent = \case
          CostUnknown -> writeIORef warned True
          _ -> pure ()}
    _ <- runTurn env {envHooks = hooks} [] "go"
    readIORef warned `shouldReturn` True

  it "stops with the provider's error" $ do
    (complete, _) <- scripted []
    out <- runTurn (mkEnv complete Yolo True) [] "go"
    outStop out `shouldBe` Failed "script exhausted"
