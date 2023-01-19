module User where

import Message

import System.Random
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Data.Foldable (for_, forM_)
import Control.Monad (forever)
import GHC.MVar

data User = User {name :: String} deriving (Show)
data Msg = Msg {sender :: String, receiver :: String, message :: String} deriving (Show)

users :: [User]
users = [User "John",
         User "Mary",
         User "Alice",
         User "Bob",
         User "David",
         User "Stephanie",
         User "George",
         User "Scott",
         User "Emma",
         User "Tina"]

selectUser :: [User] -> IO User
selectUser xs = do
  i <- randomRIO (0, (length xs) - 1)
  let x = xs !! i 
  return x

removeItem :: String -> [User] -> [User]
removeItem _ []                 = []
removeItem x (y:ys) | x == (name y)    = removeItem x ys
                    | otherwise = y : removeItem x ys

sleepMs n = threadDelay (n * 1000)

senderReceiver :: IO ()
senderReceiver = do
     sender <- selectUser users
     let removedList = removeItem (name sender) users
     recipient <- selectUser removedList
     msg <- printMessage (name sender) (name recipient)
     print msg

     