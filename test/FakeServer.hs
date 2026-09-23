-- | A minimal HTTP server for provider tests: canned responses in order,
-- one per connection, on 127.0.0.1.
module FakeServer
  ( Canned (..)
  , json
  , freePort
  , withServer
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import Data.IORef
import Network.Socket
import qualified Network.Socket.ByteString as NSB

data Canned = Canned
  { cannedStatus :: Int
  , cannedType   :: BS.ByteString
  , cannedBody   :: BS.ByteString
  }

json :: Int -> BS.ByteString -> Canned
json code = Canned code "application/json"

-- | A port nothing listens on, at least until someone binds it.
freePort :: IO PortNumber
freePort = bracket open close socketPort
  where
    open = do
      s <- socket AF_INET Stream defaultProtocol
      bind s (SockAddrInet 0 localhost)
      pure s

-- | Start serving on @port@ after @delay@ microseconds and run the action
-- meanwhile. Returns its result and the number of requests served.
withServer :: PortNumber -> Int -> [Canned] -> IO a -> IO (a, Int)
withServer port delay responses act = do
  queue <- newIORef responses
  count <- newIORef (0 :: Int)
  t <- forkIO $ do
    threadDelay delay
    bracket open close (serve queue count)
  r <- act `finally` killThread t
  (,) r <$> readIORef count
  where
    open = do
      s <- socket AF_INET Stream defaultProtocol
      setSocketOption s ReuseAddr 1
      bind s (SockAddrInet port localhost)
      listen s 8
      pure s
    serve queue count sock = forever $ do
      (c, _) <- accept sock
      readRequest c
      modifyIORef' count (+ 1)
      canned <- atomicModifyIORef' queue $ \case
        (x : xs) -> (xs, x)
        [] -> ([], json 500 "{}")
      NSB.sendAll c (render canned) `finally` close c

localhost :: HostAddress
localhost = tupleToHostAddress (127, 0, 0, 1)

-- | Read headers and a Content-Length body, so closing sends no reset.
readRequest :: Socket -> IO ()
readRequest c = go BS.empty
  where
    go buf = case BS.breakSubstring "\r\n\r\n" buf of
      (headers, rest)
        | not (BS.null rest) -> drain (contentLength headers - (BS.length rest - 4))
        | otherwise -> NSB.recv c 4096 >>= \chunk -> if BS.null chunk then pure () else go (buf <> chunk)
    drain n
      | n <= 0 = pure ()
      | otherwise = NSB.recv c 4096 >>= \chunk -> if BS.null chunk then pure () else drain (n - BS.length chunk)
    contentLength headers =
      case [v | l <- BS8.lines headers, let (k, v) = BS8.break (== ':') l, BS8.map toLower k == "content-length"] of
        (v : _) -> maybe 0 fst (BS8.readInt (BS8.dropWhile (`elem` (": " :: String)) v))
        [] -> 0

render :: Canned -> BS.ByteString
render (Canned code ctype body) =
  BS.concat
    [ "HTTP/1.1 ", BS8.pack (show code), " X\r\n"
    , "Content-Type: ", ctype, "\r\n"
    , "Content-Length: ", BS8.pack (show (BS.length body)), "\r\n"
    , "Connection: close\r\n\r\n"
    , body
    ]
