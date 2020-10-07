module Case where

data Data1 = A Int | B
data Data2 = C | D

foo x = case x of
  A -> True
  B -> False

main = putStrLn "hi"
