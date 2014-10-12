module HMemcache.Protocol (Command(Get, Set), parseCommand) where

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.ByteString.Char8 as B8
import Data.Word

data Command = Get String | Set String String deriving Show

parseWord = Atto.takeWhile ((/=) ' ')
parseBody = Atto.takeWhile ((/=) '\n')

parseSet = do
  key <- parseWord
  Atto.char ' '
  body <- parseBody
  Atto.char '\n'
  return $ Set (B8.unpack key) (B8.unpack body)

parseGet = do
  body <- parseBody
  Atto.char '\n'
  return $ Get (unpack body)

parseCommand :: Atto.Parser Command
parseCommand = do
  cmd <- parseWord
  Atto.char ' '
  case unpack cmd of
    "get" -> parseGet
    "set" -> parseSet
    _ -> error "Unknown command"
