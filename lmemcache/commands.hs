module LMemcache.Commands (
  Command(Get, Gets, Set),
  StorageFlags, ExpTime,
  Key, Value,
  StorageCommandArgs(..),
  RetrievalCommandArgs(..)
) where

import Data.ByteString.Char8
import Data.Word

type ExpTime = Int
type StorageFlags = Word32
type Key = ByteString
type Value = ByteString
data StorageCommandArgs = StorageCommandArgs { key :: Key,
                                               flags :: StorageFlags,
                                               exptime :: ExpTime,
                                               bytes :: Int,
                                               noreply :: Bool } deriving (Show)

data RetrievalCommandArgs = RetrievalCommandArgs { keys :: [ByteString] } deriving (Show)

data Command = Set StorageCommandArgs Value | Get RetrievalCommandArgs | Gets RetrievalCommandArgs deriving (Show)
