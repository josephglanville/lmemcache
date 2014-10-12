{-# LANGUAGE DeriveDataTypeable #-}

module HMemcache.Server (ServerArgs(..), server) where

import Data.Typeable
import Data.Data
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B8
import HMemcache.Protocol
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
  maybeRes <- messageParser res sock
  case maybeRes of
    Just (ParseResult cmd rem) -> do
      putStrLn (show cmd ++ " " ++ show rem)
      receiveMessage rem sock
    Nothing -> do
      sClose sock
      putStrLn "Client disconnected!"

data ParseResult = ParseResult { command :: Command, remainder :: B8.ByteString } deriving (Show)

fmtParseError :: B8.ByteString -> [String] -> String -> String
fmtParseError remainder tokens message = concat $ intersperse "\n" [show remainder, message]

messageParser :: B8.ByteString -> Socket -> IO (Maybe ParseResult)
messageParser s sock = messageParser_ (A.parse parseCommand s) sock

messageParser_ :: A.Result Command -> Socket -> IO (Maybe ParseResult)
messageParser_ (A.Done rem res) sock = return . Just $ ParseResult res rem
messageParser_ (A.Fail rem t m) sock = putStrLn (fmtParseError rem t m) >> messageParser B8.empty sock
messageParser_ (A.Partial cont) sock = do msg <- recv sock 512
                                          if B8.null msg
                                          then return Nothing
                                          else doFeed msg
                                            where doFeed b = messageParser_ (A.feed (A.Partial cont) b) sock
