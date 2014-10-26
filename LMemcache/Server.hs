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
data ServerState = ServerState Socket StoreState

server :: ServerArgs -> IO ()
server svrargs = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol
  let opts = [(ReuseAddr, 1)]
  foldr (>>) (return ()) $ uncurry (setSocketOption sock) <$> opts
  bindSocket sock $ SockAddrInet (fromIntegral $ port svrargs) 0
  listen sock 5
  store <- newStore
  sockHandler (ServerState sock store)
  sClose sock

sockHandler :: ServerState -> IO ()
sockHandler (ServerState sock store) = do
  (sockh, _) <- accept sock
  forkIO $ clientHandler (ServerState sockh store)
  sockHandler (ServerState sock store)

-- Handles initial client connection
clientHandler :: ServerState -> IO ()
clientHandler (ServerState sock store) = do
  putStrLn "Client Connected!"
  initialBuffer <- recv sock 512
  receiveMessage (ServerState sock store) $ lineParser TextProtocol initialBuffer Start

-- Loops until Client disconnects
receiveMessage :: ServerState -> ParseState -> IO ()
receiveMessage (ServerState sock store) parseState = do
  newState <- progressParser parseState -- parses all commands in the buffer
  processCommands (ServerState sock store) (commands newState)
  -- results <- applyCommands store newState -- applies all commands gets results
  -- TODO send results to the client
  msg <- recv sock 512 -- gets a new buffer
  if B8.null msg
  then putStrLn "Client disconnected!" >> sClose sock
  else receiveMessage (ServerState sock store) $ lineParser TextProtocol msg (stripCmds newState)

-- Strips cmds out of ParseState now they have been processed
--- Should be a way to get rid of this
stripCmds ::  ParseState -> ParseState
stripCmds (Running r cmds i) = Running r [] i


processCommands :: ServerState -> [Command] -> IO ()
processCommands _ [] = return ()
processCommands (ServerState sock store) (cmd:cmds) = do
  result <- applyCommand store cmd
  putStrLn $ show result
  -- magic to marshall and send on socket
  processCommands (ServerState sock store) cmds

-- processCommand :: Command -> IO (CommandResult)

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
  return $ lineParser TextProtocol B8.empty Start

applyCommand :: StoreState -> Command -> IO (CommandResult)
applyCommand store (Get (RetrievalCommandArgs (k:_))) = do
    -- for now only fetch one key
    out <- storeLookup store k
    case out of
      Just (Entry f b v) -> return (Retrieved [(RetrievedValue k f b 0)])
      Nothing -> return NotFound
applyCommand store (Set commonArgs noReply dataBlock) = do
  storeInsert store (key commonArgs) (Entry (flags commonArgs) (bytes commonArgs) dataBlock)
  return Stored
