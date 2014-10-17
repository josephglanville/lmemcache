{-
Copyright (c) 2014 Joseph Glanville <jgp@jpg.id.au>
                   Duncan Burke <duncankburke@gmail.com>

This software may be modified and distributed under the terms
of the MIT license. See the LICENSE file for details.
-}

{- |
   module      : LMemcache.Storage
   copyright   : (c) Joseph Glanville, Duncan Burke
   license     : MIT

   maintaner   : jpg@jpg.id.au
   stability   : experimental
-}

module LMemcache.Storage (Store, StoreState(..), newStore, testStore, storeLookup, storeInsert) where

import           Control.Concurrent
import           Data.ByteString.Char8
import qualified Data.Map              as M
import           LMemcache.Commands

type Store = M.Map Key Value

newtype StoreState = StoreState (MVar Store)

newStore :: IO StoreState
newStore = do
 s <- newMVar M.empty
 return (StoreState s)

storeInsert :: StoreState -> Key -> Value -> IO ()
storeInsert (StoreState s) key value = do
  store <- takeMVar s
  putMVar s (M.insert key value store)


storeLookup :: StoreState -> Key -> IO (Maybe Value)
storeLookup (StoreState s) key = do
  store <- takeMVar s
  putMVar s store
  return (M.lookup key store)

testStore :: IO ()
testStore = do
  s <- newStore
  sequence_ [ storeInsert s (pack ("name" ++ show n)) (pack (show n)) | n <- [1..10000] ]
  storeLookup s (pack "name999") >>= print
  storeLookup s (pack "unknown") >>= print
