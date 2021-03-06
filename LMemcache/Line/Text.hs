{-
Copyright (c) 2014 Joseph Glanville <jgp@jpg.id.au>
                   Duncan Burke <duncankburke@gmail.com>

This software may be modified and distributed under the terms
of the MIT license. See the LICENSE file for details.
-}

{- |
   module      : LMemcache.Protocol
   copyright   : (c) Joseph Glanville, Duncan Burke
   license     : MIT

   maintainer  : jpg@jpg.id.au
   stability   : experimental
-}

module LMemcache.Line.Text (TextProtocol(TextProtocol), parseCommand) where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString.Char8            hiding (putStrLn)
import           Data.Char8
import           Data.Word
import           Debug.Trace
import           Text.Read
import           LMemcache.Commands
import           LMemcache.Line.Base

data TextProtocol = TextProtocol

instance Protocol TextProtocol where
  parser = parseCommand
  find_start = undefined
  marshaller = undefined


-- TODO(jpg): attoparsec has fast implementations of isSpace and friends
parseWord = A.takeWhile1 $ \c -> not $ isControl c || isSpace c
parseNumber :: Read a => A.Parser (Maybe a)
parseNumber = readMaybe <$> A.many1 A.digit

newline = A.char '\r' >> A.char '\n'

parseStdStoreCommand :: String -> A.Parser Command
parseStdStoreCommand cmd = do
  common <- parseStoreCommandArgs
  noReply <- parseNoReply
  newline
  value <- parseValue (bytes common)
  case cmd of
    "set" -> return $ Set common noReply value

parseStoreCommandArgs :: A.Parser StoreCommandArgs
parseStoreCommandArgs = do
  key <- parseWord
  A.space
  Just flags <- parseNumber
  A.space
  Just exptime <- parseNumber
  A.space
  Just bytes <- parseNumber
  return $ StoreCommandArgs key flags exptime bytes

parseNoReply :: A.Parser NoReply
parseNoReply = do
  A.option False $ A.string (pack " noreply") >> return True

parseRetrievalCommandArgs = do
  keys <- A.sepBy1 parseWord A.space
  newline
  return $ RetrievalCommandArgs keys

parseCommand :: TextProtocol -> A.Parser Command
parseCommand p = do
  cmd <- parseWord
  A.space
  case unpack cmd of
    "get" -> Get <$> parseRetrievalCommandArgs
    "gets" -> Gets <$> parseRetrievalCommandArgs
    "set" -> parseStdStoreCommand "set"
    _ -> fail "Unknown command"

parseValue :: Int -> A.Parser Value
parseValue b = do
  datablock <- A.take b
  newline
  return datablock
