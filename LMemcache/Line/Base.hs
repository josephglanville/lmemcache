{-# LANGUAGE NullaryTypeClasses #-}

module LMemcache.Line.Base (
  Protocol,
  parser,
  find_start,
  marshaller,
  lineParser,
  commands,
  ParseState(..)
) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B8
import           LMemcache.Commands
import           Control.Monad.State
import           Data.DList

class Protocol a where
  parser :: a -> A.Parser Command
  find_start :: a -> B8.ByteString -> B8.ByteString
  marshaller :: a -> CommandResult -> B8.ByteString

data ParseState = Start |
                  Running { parser_res :: A.Result Command,
                            commands   :: [Command],
                            parser_ins :: A.Parser Command } |
                  Failed { rem     :: B8.ByteString,
                           message :: String }

lineParser :: (Protocol proto) => proto -> B8.ByteString -> ParseState -> ParseState
lineParser proto new s = case s of
                              Start -> lineParser_ p [] (A.parse p new)
                              Running res cmds _ -> lineParser_ p cmds (A.feed res new)
                              Failed rem msg -> Failed rem msg
                              where p = parser proto

lineParser_ :: A.Parser Command -> [Command] -> A.Result Command -> ParseState
lineParser_ p cmds (A.Done rem res) = Running (A.parse p rem) (cmds ++ [res]) p
lineParser_ p cmds (A.Fail rem t m) = Failed rem m
lineParser_ p cmds (A.Partial cont) = Running (A.Partial cont) cmds p

-- TODO abstract marshaller
