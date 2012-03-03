module Main where

data Color = Red | Green | Blue
  deriving (Show, Read)

data Person = Person
  { pAge :: Int
  , pShirtColor :: Color
  }
  deriving (Show, Read)

personA :: Person
personA = Person 20 Blue

main = do
  -- writeFile "personA.txt" $ show personA
  personB <- (read `fmap` readFile "personA.txt")::IO Person
  putStrLn $ show personB
