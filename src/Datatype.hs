module Datatype (
  User(..),
  Msg(..),
  users,
)where

data User = User {name :: String} deriving (Show)
data Msg = Msg {sender :: User, receiver :: User, message :: String } deriving (Show)

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