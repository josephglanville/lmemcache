{-# LANGUAGE DeriveDataTypeable #-}
{-
Copyright (c) 2014 Joseph Glanville <jgp@jpg.id.au>
                   Duncan Burke <duncankburke@gmail.com>

This software may be modified and distributed under the terms
of the MIT license. See the LICENSE file for details.
-}

{- |
   module      : LMemcache.Server
   copyright   : (c) Joseph Glanville, Duncan Burke
   license     : MIT

   maintaner   : Joseph Glanville
   stability   : experimental
-}

module LMemcache.Server (ServerArgs(..), server) where

import           Control.Applicative              hiding (empty)
import           Control.Concurrent
import           Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B8
import           Data.Data
import           Data.List
import qualified Data.Map                         as M
import           Data.Typeable
import           Debug.Trace
import           LMemcache.Commands
import           LMemcache.Line.Base
import           LMemcache.Line.Text
import           LMemcache.Storage
import           Network.Socket                   hiding (recv, send)
import           Network.Socket.ByteString

data ServerArgs = ServerArgs { port :: Int } deriving (Show, Data, Typeable)

server :: ServerArgs -> IO ()
server svrargs = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol
  let opts = [(ReuseAddr, 1)]
  foldr (>>) (return ()) $ uncurry (setSocketOption sock) <$> opts
  bindSocket sock $ SockAddrInet (fromIntegral $ port svrargs) 0
  listen sock 5
  store <- newStore
  sockHandler store sock
  sClose sock

sockHandler :: StoreState -> Socket -> IO ()
sockHandler store sock = do
  (sockh, _) <- accept sock
  forkIO $ putStrLn "Client Connected!" >> receiveMessage store sockh Start
  sockHandler store sock

receiveMessage :: StoreState -> Socket -> ParseState -> IO ()
receiveMessage store sock parseState = do
  newState <- applyAll store parseState
  msg <- recv sock 512
  if B8.null msg
  then putStrLn "Client disconnected!" >> sClose sock
  else receiveMessage store sock (lineParser TextProtocol msg newState)

applyAll :: StoreState -> ParseState -> IO (ParseState)
applyAll store (Running res cmds p) = do
  applyCommands store cmds
  case res of
    A.Done rem _ -> do
      applyAll store $ lineParser TextProtocol rem Start
    A.Partial _ -> do
      return $ Running res [] p

applyAll store (Failed _ _) = do
  putStrLn "Client sent bad command. :("
  return Start

applyAll store (Start) = return Start

applyCommands :: StoreState -> [Command] -> IO ()
applyCommands store cmds = do
  forM_ cmds $ \c -> do
    case c of
      Get r -> do
        forM_ (keys r) $ \k -> do
          putStrLn $ show k
          out <- storeLookup store k
          case out of
            Just v -> putStrLn $ show v
            Nothing -> putStrLn "Not found"
      Set s d -> do
        storeInsert store (key s) d
