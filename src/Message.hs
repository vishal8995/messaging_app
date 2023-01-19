module Message where

printMessage :: String -> String -> IO String
printMessage sender receiver = do
     let msg = "Hello " ++ (receiver) ++ " this is a message from " ++ (sender)
     return msg