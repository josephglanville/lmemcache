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
  Command(..),
  Key, Value, NoReply, Flags, ExpTime, Bytes,
  StoreCommandArgs(StoreCommandArgs),
  RetrievalCommandArgs(RetrievalCommandArgs),
  CommandResult,
  bytes, key, keys, exptime, flags
) where

import           Data.ByteString.Char8
import           Data.Typeable
import           Data.Word

type ExpTime = Int
type Flags = Word32
type Key = ByteString
type Value = ByteString
type Bytes = Int
type Cas = Int
type NoReply = Bool

data StoreCommandArgs = StoreCommandArgs { key     :: Key,
                                           flags   :: Flags,
                                           exptime :: ExpTime,
                                           bytes   :: Bytes } deriving (Show)

data RetrievalCommandArgs = RetrievalCommandArgs { keys :: [ByteString] } deriving (Show)

data Command = Set StoreCommandArgs NoReply Value |
               Add StoreCommandArgs NoReply Value |
               Replace StoreCommandArgs NoReply Value |
               Append StoreCommandArgs NoReply Value |
               Prepend StoreCommandArgs NoReply Value |
               Cas StoreCommandArgs Cas NoReply Value |
               Get RetrievalCommandArgs |
               Gets RetrievalCommandArgs deriving (Show, Typeable)

data RetrievedValue = RetrievedValue Key Flags Bytes Cas deriving (Show)

data CommandResult = Stored | -- successful Set/Cas/Add/Replace
                     Retrieved [RetrievedValue] | -- successful Get/Gets or Inc/Dec
                     Deleted | -- successful Delete
                     Touched | -- successful Touch
                     NotStored | -- failed Add/Replace
                     Exists | -- failed Cas because key modified since
                     NotFound -- failed Cas/Inc/Dec/Touch because key not found
