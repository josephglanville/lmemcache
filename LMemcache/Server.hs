{-# LANGUAGE DeriveDataTypeable #-}

module LMemcache.Server (ServerArgs(..), server) where

import LMemcache.Commands
import LMemcache.Protocol
import LMemcache.Parser
import LMemcache.Storage
import qualified Data.ByteString.Char8 as B8
import Data.Typeable
import Data.Data
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Control.Concurrent (forkIO)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Debug.Trace
import Data.List
import qualified Data.Map as M
import Control.Applicative hiding (empty)
import Control.Concurrent

data ServerArgs = ServerArgs { port :: Int } deriving (Show, Data, Typeable)

server :: ServerArgs -> IO ()
server svrargs = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol
  let opts = [(ReuseAddr, 1)]
  foldr (>>) (return ()) $ uncurry (setSocketOption sock) <$> opts
  bindSocket sock $ SockAddrInet (fromIntegral $ port svrargs) 0
  listen sock 5
  store <- newStore
  testStore
  sockHandler store sock
  sClose sock

sockHandler :: StoreState -> Socket -> IO ()
sockHandler store sock = do
  (sockh, _) <- accept sock
  forkIO $ putStrLn "Client connected!" >> receiveMessage store B8.empty sockh
  sockHandler store sock

receiveMessage :: StoreState -> B8.ByteString -> Socket -> IO ()
receiveMessage store res sock = do
  maybeRes <- parser res sock
  case maybeRes of
    Just (ParseResult cmd rem) -> do
      applyCommand store cmd
      putStrLn (show cmd ++ " " ++ show rem)
      receiveMessage store rem sock
    Nothing -> do
      sClose sock
      putStrLn "Client disconnected!"

-- should actually return some datatype that represents a response
applyCommand :: StoreState -> Command -> IO (B8.ByteString)
applyCommand store cmd = do
  case cmd of
    Get r -> undefined
    Gets r -> undefined
    Set s d -> undefined