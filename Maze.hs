module Main where

import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 10000 $ [ if x == 0 then '/' else '\\' | x <- ((randomRs (0, 1) gen)::[Int]) ]
