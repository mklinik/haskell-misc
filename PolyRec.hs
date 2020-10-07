module Main where

data ABList a b = Nil | ABCons a (ABList b a)
  deriving Show

abLength Nil = 0
abLength (ABCons _ rest) = baLength rest + 1

baLength Nil = 0
baLength (ABCons _ rest) = abLength rest + 1
