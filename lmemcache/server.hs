{-# LANGUAGE DeriveDataTypeable #-}

module LMemcache.Server (ServerArgs(..), server) where

import LMemcache.Commands
import LMemcache.Protocol
import LMemcache.Parser
import qualified Data.ByteString.Char8 as B8
import Data.Typeable
import Data.Data
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Control.Concurrent (forkIO)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Debug.Trace
import Data.List
import Control.Applicative

data ServerArgs = ServerArgs { port :: Int } deriving (Show, Data, Typeable)

server :: ServerArgs -> IO ()
server svrargs = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol
  let opts = [(ReuseAddr, 1)]
  foldr (>>) (return ()) (uncurry (setSocketOption sock) <$> opts)
  bindSocket sock $ SockAddrInet (fromIntegral $ port svrargs) 0
  listen sock 5
  sockHandler sock
  sClose sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
  (sockh, _) <- accept sock
  forkIO $ putStrLn "Client connected!" >> receiveMessage B8.empty sockh
  sockHandler sock

receiveMessage :: B8.ByteString -> Socket -> IO ()
receiveMessage res sock = do
  maybeRes <- parser res sock
  case maybeRes of
    Just (ParseResult cmd rem) -> do
      putStrLn (show cmd ++ " " ++ show rem)
      receiveMessage rem sock
    Nothing -> do
      sClose sock
      putStrLn "Client disconnected!"
