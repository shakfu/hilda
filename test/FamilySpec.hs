module FamilySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.MVar
import Control.Exception (onException)
import Control.Monad (when)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Types (parseMaybe, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hilda.Agent
import Hilda.Family
import Hilda.Policy
import Hilda.Tools (builtinTools, resultLimit)
import Hilda.Types
import System.Timeout (timeout)
import Test.Hspec

-- | The first user message: the root's prompt or a child's task.
taskOf :: Request -> Text
taskOf r = case [t | User t <- reqMessages r] of
  (t : _) -> t
  [] -> ""

-- | A backend with one script per task, recording every request.
routed :: [(Text, [Reply])] -> IO (Complete, IO [Request])
routed scripts = do
  queues <- newIORef (Map.fromList scripts)
  seen <- newIORef []
  let complete _ r = do
        atomicModifyIORef' seen (\xs -> (r : xs, ()))
        atomicModifyIORef' queues $ \m -> case Map.lookup (taskOf r) m of
          Just (x : xs) -> (Map.insert (taskOf r) xs m, Right x)
          _ -> (m, Left ("script exhausted for " <> taskOf r))
  pure (complete, reverse <$> readIORef seen)

reply :: Maybe Text -> [ToolCall] -> Reply
reply t cs = Reply t cs (Usage 10 5 0 Nothing) [] FinishStop

call :: Text -> Text -> Value -> ToolCall
call i name args = ToolCall i name (T.pack (BL.unpack (encode args)))

agentCall :: Text -> Text -> ToolCall
agentCall i task = call i "agent" (object ["task" .= task])

quiet :: Hooks IO
quiet = Hooks {onEvent = const (pure ()), confirm = const (pure False)}

-- | A root env in a family over @complete@.
rootEnv :: Family -> Mode -> Hooks IO -> Env IO
rootEnv fam mode hooks =
  attach (Just fam) "sys" $
    Env
      { envComplete = familyComplete fam
      , envModel = "test-model"
      , envTools = builtinTools resultLimit
      , envMode = mode
      , envMaxTurns = 10
      , envBudget = 1000000
      , envCostLimit = Nothing
      , envSpent = 0
      , envHooks = hooks
      }

runRoot :: Env IO -> IO Outcome
runRoot env = runTurn env [System "sys"] "root"

toolResults :: [Message] -> [(Text, Text)]
toolResults hist = [(i, c) | ToolResult i c <- hist]

toolNames :: Request -> [Text]
toolNames = mapMaybe (parseMaybe (withObject "spec" (\o -> o .: "function" >>= withObject "fn" (.: "name")))) . reqTools

-- | A request with about @n@ tokens of history.
sized :: Int -> Request
sized n = Request "m" [User (T.replicate (4 * n) "x")] [] Nothing

spec :: Spec
spec = do
  describe "agent tool" $ do
    it "returns the child's answer as the parent's tool result" $ do
      (complete, _) <- routed [("root", [reply Nothing [agentCall "c1" "count"], reply (Just "done") []]), ("count", [reply (Just "42") []])]
      fam <- newFamily 1000000 Nothing 2 complete
      out <- runRoot (rootEnv fam Yolo quiet)
      outText out `shouldBe` "done"
      toolResults (outHistory out) `shouldBe` [("c1", "42")]

    it "runs agent calls in one reply concurrently" $ do
      (inner, _) <- routed [("root", [reply Nothing [agentCall "a" "a", agentCall "b" "b"], reply (Just "done") []]), ("a", [reply (Just "A") []]), ("b", [reply (Just "B") []])]
      a <- newEmptyMVar
      b <- newEmptyMVar
      -- Each child waits for the other's request: sequential calls deadlock.
      let complete sink r = do
            when (taskOf r == "a") (putMVar a () >> readMVar b)
            when (taskOf r == "b") (putMVar b () >> readMVar a)
            inner sink r
      fam <- newFamily 1000000 Nothing 2 complete
      out <- timeout 5000000 (runRoot (rootEnv fam Yolo quiet))
      fmap (toolResults . outHistory) out `shouldBe` Just [("a", "A"), ("b", "B")]

    it "gives a read-only parent's child only read and agent" $ do
      (complete, requests) <- routed [("root", [reply Nothing [agentCall "c1" "t"], reply (Just "done") []]), ("t", [reply (Just "ok") []])]
      fam <- newFamily 1000000 Nothing 2 complete
      _ <- runRoot (rootEnv fam ReadOnly quiet)
      map toolNames . filter ((== "t") . taskOf) <$> requests `shouldReturn` [["read", "agent"]]

    it "withholds the agent tool at the depth limit" $ do
      (complete, requests) <- routed [("root", [reply Nothing [agentCall "c1" "t"], reply (Just "done") []]), ("t", [reply (Just "ok") []])]
      fam <- newFamily 1000000 Nothing 1 complete
      _ <- runRoot (rootEnv fam Yolo quiet)
      rs <- requests
      map toolNames (filter ((== "root") . taskOf) rs) `shouldSatisfy` all ("agent" `elem`)
      map toolNames (filter ((== "t") . taskOf) rs) `shouldBe` [["read", "write", "edit", "bash"]]

    it "lets a child delegate below the depth limit" $ do
      (complete, requests) <- routed
        [ ("root", [reply Nothing [agentCall "c1" "t"], reply (Just "done") []])
        , ("t", [reply Nothing [agentCall "c2" "u"], reply (Just "ok") []])
        , ("u", [reply (Just "deep") []])
        ]
      fam <- newFamily 1000000 Nothing 2 complete
      out <- runRoot (rootEnv fam Yolo quiet)
      outText out `shouldBe` "done"
      rs <- requests
      map toolNames (filter ((== "u") . taskOf) rs) `shouldBe` [["read", "write", "edit", "bash"]]
      [c | ToolResult _ c <- concatMap reqMessages (filter ((== "t") . taskOf) rs)] `shouldBe` ["deep"]

    it "asks an ask-mode parent for a child's calls, one at a time, with tagged ids" $ do
      let write i = call i "write" (object ["path" .= ("/nonexistent/x" :: Text), "content" .= ("x" :: Text)])
      (complete, _) <- routed
        [ ("root", [reply Nothing [agentCall "a" "a", agentCall "b" "b"], reply (Just "done") []])
        , ("a", [reply Nothing [write "w1"], reply (Just "A") []])
        , ("b", [reply Nothing [write "w1"], reply (Just "B") []])
        ]
      asked <- newIORef []
      inFlight <- newIORef (0 :: Int)
      peak <- newIORef (0 :: Int)
      let hooks =
            quiet
              { confirm = \c -> do
                  n <- atomicModifyIORef' inFlight (\k -> (k + 1, k + 1))
                  modifyIORef' peak (max n)
                  threadDelay 20000
                  atomicModifyIORef' inFlight (\k -> (k - 1, ()))
                  atomicModifyIORef' asked (\xs -> (callId c : xs, ()))
                  pure False
              }
      fam <- newFamily 1000000 Nothing 2 complete
      _ <- runRoot (rootEnv fam Ask hooks)
      ids <- readIORef asked
      ids `shouldMatchList` ["agent-1.1/w1", "agent-2.1/w1"]
      readIORef peak `shouldReturn` 1

    it "reports a child's failure as the tool's error" $ do
      (complete, _) <- routed [("root", [reply Nothing [agentCall "c1" "t"], reply (Just "done") []])]
      fam <- newFamily 1000000 Nothing 2 complete
      out <- runRoot (rootEnv fam Yolo quiet)
      outText out `shouldBe` "done"
      map snd (toolResults (outHistory out)) `shouldSatisfy` all ("error: agent-1 failed: script exhausted" `T.isPrefixOf`)

    it "tags child events with the child's label" $ do
      (complete, _) <- routed
        [ ("root", [reply Nothing [agentCall "c1" "t"], reply (Just "done") []])
        , ("t", [reply Nothing [call "r1" "read" (object ["path" .= ("/nonexistent" :: Text)])], reply (Just "ok") []])
        ]
      seen <- newIORef []
      let hooks = quiet {onEvent = \case Sub l (CallFinished c _) -> modifyIORef' seen ((l, callId c) :); _ -> pure ()}
      fam <- newFamily 1000000 Nothing 2 complete
      _ <- runRoot (rootEnv fam Yolo hooks)
      readIORef seen `shouldReturn` [("agent-1", "agent-1.1/r1")]

    it "stops a child once the family reaches --max-cost" $ do
      let costly = Reply Nothing [agentCall "c1" "t"] (Usage 10 5 0 (Just 1.0)) [] FinishStop
      (complete, _) <- routed [("root", [costly, reply (Just "done") []]), ("t", [reply (Just "ok") []])]
      fam <- newFamily 1000000 (Just 1.0) 2 complete
      out <- runRoot (rootEnv fam Yolo quiet)
      map snd (toolResults (outHistory out)) `shouldSatisfy` all ("error: agent-1 failed: the family reached its cost limit" `T.isPrefixOf`)

    it "stops the children when the parent's turn is cancelled" $ do
      (inner, _) <- routed [("root", [reply Nothing [agentCall "c1" "t"]])]
      arrived <- newEmptyMVar
      killed <- newEmptyMVar
      let complete sink r
            | taskOf r == "t" = (putMVar arrived () >> threadDelay 60000000 >> inner sink r) `onException` putMVar killed ()
            | otherwise = inner sink r
      fam <- newFamily 1000000 Nothing 2 complete
      root <- async (runRoot (rootEnv fam Yolo quiet))
      takeMVar arrived
      cancel root
      timeout 2000000 (takeMVar killed) `shouldReturn` Just ()

  describe "budget" $ do
    it "sends the grant as the output cap" $ do
      seen <- newIORef []
      let base _ r = Right (reply (Just "ok") []) <$ modifyIORef' seen (r :)
      big <- newFamily 1000000 Nothing 2 base
      _ <- familyComplete big (const (pure ())) (sized 100)
      small <- newFamily 20000 Nothing 2 base
      _ <- familyComplete small (const (pure ())) (sized 100)
      caps <- map reqMaxTokens . reverse <$> readIORef seen
      caps `shouldBe` [Just ceilingTokens, Just (19000 - 100)]

    it "fails a request that settled spending leaves no room for" $ do
      calls <- newIORef (0 :: Int)
      let base _ _ = Right (reply (Just "ok") []) <$ modifyIORef' calls (+ 1)
      fam <- newFamily 5000 Nothing 2 base
      r <- familyComplete fam (const (pure ())) (sized 1000)
      either T.unpack (const "") r `shouldSatisfy` ("the family token budget is spent" `isPrefixOf`)
      readIORef calls `shouldReturn` 0

    it "settles a reply at its reported usage, else at the estimate" $ do
      let reported = Reply (Just "ok") [] (Usage 70 30 0 Nothing) [] FinishStop
      fam <- newFamily 1000000 Nothing 2 (\_ _ -> pure (Right reported))
      _ <- familyComplete fam (const (pure ())) (sized 100)
      familySpent fam `shouldReturn` 100
      familyHeld fam `shouldReturn` 0
      bare <- newFamily 1000000 Nothing 2 (\_ _ -> pure (Right (Reply (Just "abcd") [] mempty [] FinishStop)))
      _ <- familyComplete bare (const (pure ())) (sized 100)
      familySpent bare `shouldReturn` 101

    it "settles a failed request at its full hold" $ do
      fam <- newFamily 20000 Nothing 2 (\_ _ -> pure (Left "boom"))
      _ <- familyComplete fam (const (pure ())) (sized 100)
      familySpent fam `shouldReturn` 19000
      familyHeld fam `shouldReturn` 0

    it "waits while other holds fill the budget, then proceeds" $ do
      release <- newEmptyMVar
      started <- newIORef (0 :: Int)
      let base _ r = do
            n <- atomicModifyIORef' started (\k -> (k + 1, k + 1))
            when (n == 1) (readMVar release)
            pure (Right (Reply (Just "ok") [] (Usage (length (show r) `min` 10) 5 0 Nothing) [] FinishStop))
      fam <- newFamily 20000 Nothing 2 base
      first' <- async (familyComplete fam (const (pure ())) (sized 100))
      threadDelay 50000
      second <- async (familyComplete fam (const (pure ())) (sized 100))
      threadDelay 100000
      readIORef started `shouldReturn` 1
      putMVar release ()
      _ <- wait first'
      r <- timeout 2000000 (wait second)
      fmap (either Just (const Nothing)) r `shouldBe` Just Nothing
      readIORef started `shouldReturn` 2

    it "settles a 4xx refusal at nothing" $ do
      fam <- newFamily 20000 Nothing 2 (\_ _ -> pure (Left "HTTP 400: bad parameter"))
      _ <- familyComplete fam (const (pure ())) (sized 100)
      familySpent fam `shouldReturn` 0
      familyHeld fam `shouldReturn` 0

    it "settles a cancelled request at its full hold" $ do
      fam <- newFamily 20000 Nothing 2 (\_ _ -> threadDelay 60000000 >> pure (Left "late"))
      req <- async (familyComplete fam (const (pure ())) (sized 100))
      let held = familyHeld fam >>= \h -> if h > 0 then pure h else threadDelay 1000 >> held
      _ <- held
      cancel req
      familyHeld fam `shouldReturn` 0
      familySpent fam `shouldReturn` 19000
