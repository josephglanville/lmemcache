
module LMemcache.Line.Base (Protocol) where
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B8

import Data.DList
import Control.Monad.State

class Protocol where
  parser :: A.Parser Command
  find_start :: B8.ByteString -> B8.ByteString
  marshaller :: CommandResponse -> B8.ByteString

data ParseState = Start |
                  Running { parser_res :: A.Result,
                            commands :: [Command] } |
                  Failed { rem :: B8.ByteString,
                           message :: String }

parser :: (Protocol Proto) => Proto -> B8.ByteString -> ParseState -> ParseState
parser proto newdata state  = case state of
                                Start -> parser_ [] $ A.parse p newdata
                                Running res com -> parser_ com $ A.feed p res newdata
                                Failed -> fail "handle your damn failure"
                              where p = parser proto

parser_ :: [Command] -> A.Result -> ParseState
parser_ com (A.Done rem res) = 
parser_ com (A.Fail rem t m) =
parser_ com (A.Partial cont) =



parser_ (A.parse parseCommand s) sock

parser_ :: A.Result Command -> Socket -> IO (Maybe ParseResult)
parser_ (A.Done rem res) sock = return . Just $ ParseResult res rem
parser_ (A.Fail rem t m) sock = putStrLn (fmtParseError rem t m) >> parser B8.empty sock
parser_ (A.Partial cont) sock = do msg <- recv sock 512
                                   if B8.null msg
                                   then return Nothing
                                   else doFeed msg
                                     where doFeed b = parser_ (A.feed (A.Partial cont) b) sock
