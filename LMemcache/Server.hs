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

   maintainer  : Joseph Glanville
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
  forkIO $ clientHandler store sockh
  sockHandler store sock

-- Handles initial client connection
clientHandler :: StoreState -> Socket -> IO ()
clientHandler store sock = do
  putStrLn "Client Connected!"
  initialBuffer <- recv sock 512
  receiveMessage store sock $ lineParser TextProtocol initialBuffer Start

-- Loops until Client disconnects
receiveMessage :: StoreState -> Socket -> ParseState -> IO ()
receiveMessage store sock parseState = do
  newState <- progressParser parseState -- parses all commands in the buffer
  results <- applyCommands store newState -- applies all commands gets results
  -- TODO send results to the client
  msg <- recv sock 512 -- gets a new buffer
  if B8.null msg
  then putStrLn "Client disconnected!" >> sClose sock
  else receiveMessage store sock (lineParser TextProtocol msg newState)

-- Loops until parser returns Partial or Failed
progressParser :: ParseState -> IO (ParseState)
progressParser (Running res cmds p) = do
  case res of
    A.Done rem _ -> do
      progressParser $ lineParser TextProtocol rem (Running res cmds p)
    A.Partial _ -> do
      return $ Running res cmds p
progressParser (Failed _ _) = do
  putStrLn "Client sent bad command. :("
  return Start

-- Might be able to eliminate this
applyCommands :: StoreState -> ParseState -> IO ()
applyCommands store (Running _ cmds _ ) = do
  forM_ cmds $ \c -> applyCommand store c
applyCommands store (Start) = do
  putStrLn "Parser restarted"

applyCommand :: StoreState -> Command -> IO ()
applyCommand store (Get r) = do
  forM_ (keys r) $ \k -> do
    putStrLn $ show k
    out <- storeLookup store k
    case out of
      Just v -> putStrLn $ show v
      Nothing -> putStrLn "Not Found"

applyCommand store (Set s n d) = do
  storeInsert store (key s) d
  putStrLn "Stored"
