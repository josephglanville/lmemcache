{-# LANGUAGE NullaryTypeClasses #-}

module LMemcache.Line.Base (Protocol) where
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B8
import           LMemcache.Commands
import           LMemcache.Responses

import           Control.Monad.State
import           Data.DList

class Protocol where
  parser :: A.Parser Command
  find_start :: B8.ByteString -> B8.ByteString
  marshaller :: CommandResponse -> B8.ByteString

data ParseState = Start |
                  Running { parser_res :: A.Result Command,
                            commands   :: [Command] } |
                  Failed { rem     :: B8.ByteString,
                           message :: String }

-- line_parser :: (Protocol p) => p -> B8.ByteString -> ParseState -> ParseState
-- line_parser :: (Protocol proto) => proto -> B8.ByteString -> ParseState -> ParseState
line_parser proto newdata state  = case state of
                                Start -> line_parser_ [] $ A.parse p newdata
                                Running res com -> line_parser_ com $ A.feed p res newdata
                                Failed -> fail "handle your damn failure"
                              where p = parser proto

line_parser_ :: [Command] -> A.Result Command -> ParseState
line_parser_ com (A.Done rem res) = Running (A.parse rem) (com ++ res)
line_parser_ com (A.Fail rem t m) = Failed rem m
line_parser_ com (A.Partial cont) = Running cont com
