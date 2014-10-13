module LMemcache.Storage () where

import LMemcache.Protocol
import Data.Map

type Store = Map Key Value

newtype StoreState = StoreState (MVar Store)
