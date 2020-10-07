module Main where

main = putStrLn "Hello World"

data Unicorn a b = Unicorn (a -> b)
