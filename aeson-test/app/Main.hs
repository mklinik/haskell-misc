{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Data.Text

data Person = Person {
      name :: String
    , age  :: Int
    } deriving (Generic, Show)

instance ToJSON Person
instance FromJSON Person

main :: IO ()
main = do
  BS.putStrLn $ encode (Person {name = "Joe", age = 12})
