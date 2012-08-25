module Main where

import           Util
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mapM_ printGPXStatistics
