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
  -- Create a list of 10 users
  users <- mapM createUser [0..9]

  -- Create an MVar to store the total message count
  messageCount <- newMVar 0

  -- Create an MVar to store all the messages
  messages <- newMVar []

  -- Spawn a thread for each user
  forM_ users $ \user -> do
    forkIO $ sendMessages user users messageCount messages

  threadDelay 5000000
  putStrLn "Completed sending all the messages!!"
  putStrLn "Calculating all the count of messages sent by each user!!"
  -- Wait for all threads to finish
  threadDelay 300000
  putStrLn "All threads finished. Final message count:"

  -- Print final message count for each user
  finalMsgs <- readMVar messages
  let msgCount = [length $ filter ((== name user) . name . receiver) finalMsgs | user <- users]
  forM_ (zip users msgCount) $ \(user, count) ->
    putStrLn $ (name user) ++ " received " ++ (show count) ++ " messages."
  
  -- Additional Feature : Find 3 Most Active Users
  let userCountList = countReceiver finalMsgs  
  let sortedCountList = sortOn (Down . snd) (Map.toList userCountList)
  putStrLn "The top 3 most active users are :"
  putStrLn $ show $ take 3 sortedCountList

--   putStrLn "The top 3 most active users are :"
--   let sortedMsgCount = reverse $ sort (msgCount)
--   forM_ (zip users sortedMsgCount) $ \(user, count) ->
--     putStrLn $ (name user) ++ " = " ++ (show count)
--print $ take 3 sortedMsgCount
