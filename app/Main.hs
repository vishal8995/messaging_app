module Main (main) where

import User
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    x1 <- forkIO (senderReceiver)
    x2 <- forkIO (senderReceiver)
    sleepMs 1000
