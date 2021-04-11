module Main where

import Control.Monad (forever)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

logAndEcho :: Socket -> IO ()
logAndEcho ss = forever $ do
  (sc, _) <- accept ss
  printAndKick sc
  close sc
  where printAndKick sc = do
          msg <- recv sc 1024
          print msg
          sendAll sc msg

main :: IO ()
main = withSocketsDo $ do
  addressInfo <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing
                 (Just "79")
  ss <- socket (addrFamily $ head addressInfo) Stream defaultProtocol
  bind ss (addrAddress $ head addressInfo)
  listen ss 1
  logAndEcho ss
  close ss
