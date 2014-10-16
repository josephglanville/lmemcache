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

lineParser proto new s  = case s of
                                Start -> lineParser_ [] $ A.parse p new
                                Running res com -> lineParser_ com $ A.feed p res new
                                Failed -> fail "handle your damn failure"
                                where p = parser proto

lineParser_ :: [Command] -> A.Result Command -> ParseState
lineParser_ com (A.Done rem res) = Running (A.parse rem) (com ++ res)
lineParser_ com (A.Fail rem t m) = Failed rem m
lineParser_ com (A.Partial cont) = Running cont com
