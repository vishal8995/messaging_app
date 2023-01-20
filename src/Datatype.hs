{- |
   Module     : Datatype

   Maintainer : Vishal <ec21202@qmul.ac.uk>

- This module contains all the custom datatypes used all across the applciation. 
- 'User' and 'MsgStruct' are datatypes used to define the structure of the User who will use the application with message structure that will be sent.

Written by Vishal
-}

module Datatype (
  User(..),
  MsgStruct(..),
  users,
)where

{- | Datatype for User Structure  -}
data User = User {name :: String} deriving (Show, Eq, Ord)

{- | Datatype for Message Structure  -}
data MsgStruct = MsgStruct {sender :: User, receiver :: User, message :: String } deriving (Show)

{- | List of Users  -}
users :: [User]
users = [User "Rohan",
         User "Rahul",
         User "Rajesh",
         User "Raj",
         User "Prem",
         User "Vijay",
         User "Radhe",
         User "Vicky",
         User "Jay",
         User "Veeru"]