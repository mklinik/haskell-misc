module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as Csv
import qualified Data.Vector as V

import Lib

readCSV :: FilePath -> IO (Either String [(String, Int)])
readCSV fileName = do
  contents <- B.readFile fileName
  -- Csv.decode does type-driven decoding to produce a vector of the given type
  return $ V.toList <$> Csv.decode Csv.HasHeader contents

doReadCSV :: FilePath -> IO [(String, Int)]
doReadCSV fileName = do
  result <- readCSV fileName
  case result of
    Left err -> error err
    Right result -> return result

main :: IO ()
main = do
  records <- doReadCSV "test.csv"
  print records
