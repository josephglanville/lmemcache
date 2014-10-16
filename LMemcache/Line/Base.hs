{-# LANGUAGE NullaryTypeClasses #-}

module LMemcache.Line.Base (Protocol, parser, find_start, marshaller) where
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B8
import           LMemcache.Commands
import           LMemcache.Responses

import           Control.Monad.State
import           Data.DList

class Protocol a where
  parser :: a -> A.Parser Command
  find_start :: a -> B8.ByteString -> B8.ByteString
  marshaller :: a -> CommandResponse -> B8.ByteString

data ParseState = Start |
                  Running { parser_res :: A.Result Command,
                            commands   :: [Command],
                            parser_ins :: A.Parser Command } |
                  Failed { rem     :: B8.ByteString,
                           message :: String }

lineParser :: (Protocol proto) => proto -> B8.ByteString -> ParseState -> ParseState
lineParser proto new s = case s of
                              Start -> lineParser_ p [] (A.parse p new)
                              Running res com _ -> lineParser_ p com (A.feed res new)
                              Failed rem msg -> Failed rem msg
                              where p = parser proto

lineParser_ :: A.Parser Command -> [Command] -> A.Result Command -> ParseState
lineParser_ p com (A.Done rem res) = Running (A.parse p rem) (com ++ [res]) p
lineParser_ p com (A.Fail rem t m) = Failed rem m
lineParser_ p com (A.Partial cont) = Running (A.Partial cont) com p
