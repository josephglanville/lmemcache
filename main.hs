module Main (main) where

import           LMemcache.Server
import           System.Console.CmdArgs.Implicit

serverArgs = ServerArgs{ port = 8000 &= help "Port to listen on" }

main :: IO ()
main = do srvargs <- cmdArgs serverArgs
          server srvargs
