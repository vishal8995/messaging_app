{- |
   Module     : Main

   Maintainer : Vishal <ec21202@qmul.ac.uk>

   This application mimics a messaging app by sending 100 messages using 10 threads for 10 pre-defined users.
   The Application when run generates below mentioned outputs:
   - 100 instances of a Sender sending a message from the pre-defined pool to the Receiver.
   - Counts of the messages received by each specific user.
   - The Top 3 most active users, i.e., the Users who received the most messages -- {Additional Feature}

Written by Vishal
-}

module Main (main) where

import Datatype
import Functions
import Control.Concurrent
import Control.Monad
import System.Random
import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.Map as Map


main :: IO ()
main = do
  {- | Creating a list of 10 users by selecting users from the User List -}
  users <- mapM selectUser [0..9]

  {- | MVar created to store message count  -}
  msgsCount <- newMVar 0

  {- | MVar created to store all messages sent  -}
  messages <- newMVar []

  {- | Creating threads for each user by forking threads using iteration  -}
  forM_ users $ \user -> do
    forkIO $ sendMessageThreads user users msgsCount messages

  threadDelay 5000000
  putStrLn "Completed sending all messages!!"
  putStrLn ""
  putStrLn ""
  threadDelay 300000
  putStrLn "All threads are finished. Message count for each user:"
  putStrLn ""
  finalMsgs <- readMVar messages
  let msgCount = [length $ filter ((== name user) . name . receiver) finalMsgs | user <- users]
  forM_ (zip users msgCount) $ \(user, count) ->
    putStrLn $ (name user) ++ " received " ++ (show count) ++ " messages."

  {- | Additional Feature  -}
  let top3Users = take 3 $ sortOn (Down . snd) $ zip users msgCount
  putStrLn ""
  putStrLn ""
  putStrLn "Top 3 Users with most messages received:"
  putStrLn ""
  forM_ top3Users $ \(user, count) -> putStrLn $ (name user) ++ " received " ++ (show count) ++ " messages."