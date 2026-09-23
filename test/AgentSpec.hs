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
  let complete r = do
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
    , envHooks = Hooks {onEvent = const (pure ()), confirm = const (pure answer)}
    }

call :: Text -> Text -> Value -> ToolCall
call i name args = ToolCall i name (T.pack (BL.unpack (encode args)))

reply :: Maybe Text -> [ToolCall] -> Reply
reply t cs = Reply t cs (Usage 10 5)

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
    outUsage out `shouldBe` Usage 20 10
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

  it "stops with the provider's error" $ do
    (complete, _) <- scripted []
    out <- runTurn (mkEnv complete Yolo True) [] "go"
    outStop out `shouldBe` Failed "script exhausted"
