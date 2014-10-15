{-
Copyright (c) 2014 Joseph Glanville <jgp@jpg.id.au>
                   Duncan Burke <duncankburke@gmail.com>

This software may be modified and distributed under the terms
of the MIT license. See the LICENSE file for details.
-}

{- |
   module      : LMemcache.Parser
   copyright   : (c) Joseph Glanville, Duncan Burke
   license     : MIT

   maintainer  : jpg@jpg.id.au
   stability   : experimental
-}

module LMemcache.Parser (ParseResult(..), parser) where

import LMemcache.Commands
import LMemcache.Protocol
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B8
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.List

data ParseResult = ParseResult { command :: Command, remainder :: B8.ByteString } deriving (Show)

fmtParseError :: B8.ByteString -> [String] -> String -> String
fmtParseError remainder tokens message = concat $ intersperse "\n" [show remainder, message]

parser :: B8.ByteString -> Socket -> IO (Maybe ParseResult)
parser s sock = parser_ (A.parse parseCommand s) sock

parser_ :: A.Result Command -> Socket -> IO (Maybe ParseResult)
parser_ (A.Done rem res) sock = return . Just $ ParseResult res rem
parser_ (A.Fail rem t m) sock = putStrLn (fmtParseError rem t m) >> parser B8.empty sock
parser_ (A.Partial cont) sock = do msg <- recv sock 512
                                   if B8.null msg
                                   then return Nothing
                                   else doFeed msg
                                     where doFeed b = parser_ (A.feed (A.Partial cont) b) sock
