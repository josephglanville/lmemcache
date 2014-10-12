{-# LANGUAGE DeriveDataTypeable #-}

module HMemcache.Server (ServerArgs(..), server) where

import Data.Typeable
import Data.Data
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B8
import HMemcache.Protocol
import qualified Data.Attoparsec.ByteString.Char8 as Atto

data ServerArgs = ServerArgs { port :: Int } deriving (Show, Data, Typeable)

server :: ServerArgs -> IO ()
server svrargs = withSocketsDo $ do
                   sock <- socket AF_INET Stream defaultProtocol
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
  ParseResult cmd rem <- messageParser res sock
  putStrLn "Got a cmd"
  receiveMessage rem sock

data ParseResult = ParseResult { command :: Command, remainder :: B8.ByteString }

messageParser :: B8.ByteString -> Socket -> IO ParseResult
messageParser s sock = messageParser_ (Atto.parse parseCommand s) sock

messageParser_ :: Atto.Result Command -> Socket -> IO ParseResult
messageParser_ r sock = do case r of
                             Atto.Done rem res -> return $ ParseResult res rem
                             Atto.Fail rem _ _ -> undefined
                             Atto.Partial cont -> recv sock 10 >>= doFeed
                               where doFeed b = messageParser_ (Atto.feed (Atto.Partial cont) b) sock


--  B8.putStrLn msg
--  if msg == B8.pack "q" || B8.null msg
--  then sClose sockh >> putStrLn "Client disconnected"
--  else receiveMessage sockh
