module Functions (
     createUser,
     generateMessage,
     sendMessages,
     countReceiver,
)where

import Datatype

import System.Random
import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map
-- import Data.List (sortWith)

createUser :: Int -> IO User
createUser n = do
    return User {name = (name (users !! n))}

-- Function to randomly generate messages between users
generateMessage :: [User] -> IO Msg
generateMessage users = do
  sender <- randomRIO (0, (length users) - 1)
  --putStrLn ("sender: " ++ show sender)
  let withoutTheSender = filter (\x -> x /= (sender)) [0..9]
  --print ("withoutTheSender: " ++ show withoutTheSender)
  i <- randomRIO (0, (length withoutTheSender) - 1)
  --print ("i:" ++ show i)
  let receiver = withoutTheSender !! i 
  let messageList = ["Hello, how are you?", "Hi, how are you?", "Goodbye, have a nice day!", "See you later, have a nice day!"]
  j <- randomRIO (0, (length messageList) - 1)
  let text = messageList !! j
  putStrLn (name (users !! sender) ++" sent the message '" ++ text ++ "' to: " ++ name (users !! receiver))
  return $ Msg (users !! sender) (users !! receiver) text

-- Function to simulate message sending for a single user
sendMessages :: User -> [User] -> MVar Int -> MVar [Msg] -> IO ()
sendMessages user users messageCount messages = do
  -- Send 100 messages at random intervals
  forM_ [1..10] $ \_ -> do
    threadDelay =<< randomRIO (10000, 20000)
    message <- generateMessage users
    currentCount <- takeMVar messageCount
    if currentCount < 100
      then do
        putMVar messageCount (currentCount + 1)
        modifyMVar_ messages (\msgList -> return (message:msgList))
      else do
        putMVar messageCount currentCount
  putStrLn $ (name user) ++ " finished sending messages."


-- Function to find top 3 most active users (Users who received most messages)
countReceiver :: [Msg] -> Map.Map User Int
countReceiver messages = 
  let messageCounts = Map.fromList [(receiver m, 0) | m <- messages]
  in foldl (\acc m -> Map.adjust (+1) (receiver m) acc) messageCounts messages
