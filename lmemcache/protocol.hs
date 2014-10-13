module LMemcache.Protocol (Command(Get, Set), parseCommand) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 hiding (putStrLn)
import Data.Word
import Control.Applicative
import Data.Char8
import Debug.Trace
import Text.Read

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

-- TODO(jpg): attoparsec has fast implementations of isSpace and friends
parseWord = A.takeWhile1 $ \c -> not $ isControl c || isSpace c
parseNumber :: Read a => A.Parser (Maybe a)
parseNumber = readMaybe <$> A.many1 A.digit

newline = A.char '\r' >> A.char '\n'

-- TODO(jpg): read is unsafe
parseStorageCommandArgs :: A.Parser StorageCommandArgs
parseStorageCommandArgs = do
  key <- parseWord
  A.space
  Just flags <- parseNumber
  A.space
  Just exptime <- parseNumber
  A.space
  Just bytes <- parseNumber
  noreply <- A.option False $ A.string (pack " noreply") >> return True
  newline
  return $ StorageCommandArgs key flags exptime bytes noreply

parseRetrievalCommandArgs = do
  keys <- A.sepBy1 parseWord A.space
  newline
  return $ RetrievalCommandArgs keys

parseCommand :: A.Parser Command
parseCommand = do
  cmd <- parseWord
  A.space
  case unpack cmd of
    "get" -> Get <$> parseRetrievalCommandArgs
    "gets" -> Gets <$> parseRetrievalCommandArgs
    "set" -> do storeargs <- parseStorageCommandArgs
                Set storeargs <$> parseValue (bytes storeargs)
    _ -> fail "Unknown command"

parseValue :: Int -> A.Parser Value
parseValue b = do
  datablock <- A.take b
  newline
  return datablock
