module LMemcache.Storage (Store, StoreState(..), newStore, testStore) where

import Data.ByteString.Char8
import LMemcache.Commands
import qualified Data.Map as M
import Control.Concurrent

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