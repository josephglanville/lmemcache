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

   maintainer  : jpg@jpg.id.au
   stability   : experimental
-}

module LMemcache.Storage (
Store, StoreState(..), Entry(Entry),
newStore, storeLookup, storeInsert
) where

import           Control.Concurrent
import           Data.ByteString.Char8
import qualified Data.Map              as M
import           LMemcache.Commands

data Entry = Entry Flags Bytes Value deriving (Show)
type Store = M.Map Key Entry

newtype StoreState = StoreState (MVar Store)

newStore :: IO StoreState
newStore = do
 s <- newMVar M.empty
 return (StoreState s)

storeInsert :: StoreState -> Key -> Entry -> IO ()
storeInsert (StoreState s) key entry = do
  store <- takeMVar s
  putMVar s (M.insert key entry store)


storeLookup :: StoreState -> Key -> IO (Maybe Entry)
storeLookup (StoreState s) key = do
  store <- takeMVar s
  putMVar s store
  return (M.lookup key store)
