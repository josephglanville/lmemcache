module Main (main) where

import System.Console.CmdArgs.Implicit
import LMemcache.Server

serverArgs = ServerArgs{ port = 8000 &= help "Port to listen on" }

main :: IO ()
main = do srvargs <- cmdArgs serverArgs
          server srvargs
