module Main where

import qualified Control.Exception as Ex

main = do
  input <- Ex.catch getLine ((\Ex.UserInterrupt -> return "got Ctrl-C"))
  putStrLn input
