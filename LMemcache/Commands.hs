{-# LANGUAGE DeriveDataTypeable #-}
{-
Copyright (c) 2014 Joseph Glanville <jgp@jpg.id.au>
                   Duncan Burke <duncankburke@gmail.com>

This software may be modified and distributed under the terms
of the MIT license. See the LICENSE file for details.
-}

{- |
   module      : LMemcache.Commands
   copyright   : (c) Joseph Glanville, Duncan Burke
   license     : MIT

   maintainer  : jpg@jpg.id.au
   stability   : experimental
-}

module LMemcache.Commands (
  Command(Get, Gets, Set),
  StorageFlags, ExpTime,
  Key, Value,
  StorageCommandArgs(..),
  RetrievalCommandArgs(..)
) where

import           Data.ByteString.Char8
import           Data.Typeable
import           Data.Word

type ExpTime = Int
type StorageFlags = Word32
type Key = ByteString
type Value = ByteString
data StorageCommandArgs = StorageCommandArgs { key     :: Key,
                                               flags   :: StorageFlags,
                                               exptime :: ExpTime,
                                               bytes   :: Int,
                                               noreply :: Bool } deriving (Show)

data RetrievalCommandArgs = RetrievalCommandArgs { keys :: [ByteString] } deriving (Show)

data Command = Set StorageCommandArgs Value | Get RetrievalCommandArgs | Gets RetrievalCommandArgs deriving (Show, Typeable)
