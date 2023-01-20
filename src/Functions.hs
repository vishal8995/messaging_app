{- |
   Module     : Functions

   Maintainer : Vishal <ec21202@qmul.ac.uk>

- This module contains all the methods used within the application including the methods to manipulate threads, create-send messages.
Written by Vishal
-}

module Functions (
     selectUser,
     sendMessageThreads,
     countMessages,
)where

import Datatype
import System.Random
import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map


{- | Method to select user from the List of Users  -}
selectUser :: Int -> IO User
selectUser n = do
    return User {name = (name (users !! n))}


{- | Method to select messages from a pool of available messages and create message structure for the sender and the receiver  -}
createMessage :: [User] -> IO MsgStruct
createMessage users = do
  sender <- randomRIO (0, (length users) - 1)
  let excludeSender = filter (\x -> x /= (sender)) [0..9]
  i <- randomRIO (0, (length excludeSender) - 1)
  let receiver = excludeSender !! i 
  let messagePool = ["Hey, how are you?", 
                     "Shawshank Redemption is the greatest movie ever?", 
                     "The weather here in London is good now!", 
                     "You are a true friend, Thank You!",
                     "I miss home very much :("]
  j <- randomRIO (0, (length messagePool) - 1)
  let text = messagePool !! j
  putStrLn (name (users !! sender) ++" sent '" ++ text ++ "' to " ++ name (users !! receiver))
  return $ MsgStruct (users !! sender) (users !! receiver) text


{- | Method to mimic sending of message from Sender to Receiver using threads. 100 message threads are initiated at random intervals  -}
sendMessageThreads :: User -> [User] -> MVar Int -> MVar [MsgStruct] -> IO ()
sendMessageThreads user users messageCount messages = do
  forM_ [1..10] $ \_ -> do
    threadDelay =<< randomRIO (10000, 20000)
    message <- createMessage users
    msgThreadCount <- takeMVar messageCount
    if msgThreadCount < 100
      then do
        putMVar messageCount (msgThreadCount + 1)
        modifyMVar_ messages (\msgList -> return (message:msgList))
      else do
        putMVar messageCount msgThreadCount
  putStrLn "------ Thread Delay ------"


{- | Method to find top 3 most active users (Users who received most messages)  -}
countMessages :: [MsgStruct] -> Map.Map User Int
countMessages messages = 
  let msgCount = Map.fromList [(receiver m, 0) | m <- messages]
  in foldl (\acc m -> Map.adjust (+1) (receiver m) acc) msgCount messages
