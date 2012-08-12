module Main where

import           Data.Time.Clock
import           Geo.GPX.Conduit
import           Geo.Computations
import           Text.Printf
import           System.Environment (getArgs)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.List (intercalate)

main = getArgs >>= mapM_ printGPXStatistics

printGPXStatistics :: FilePath -> IO ()
printGPXStatistics file = do
  g <- readGPXFile file
  case g of
    Just gpx -> print $ gpxStatistics gpx
    Nothing  -> putStrLn $ "error reading gpx file " ++ file

type SpeedHistogram = Map Int Int

data GPXStatistics = GPXStatistics
  { statTotalDistance  :: Double -- in meters
  , statTotalTime      :: NominalDiffTime
  , statSpeedHistogram :: SpeedHistogram
  }

instance Show GPXStatistics where
  show gpxStat = "total distance: " ++ (humanReadableDistance $ statTotalDistance gpxStat)
               ++ "\ntotal time: " ++ (humanReadableDiffTime $ statTotalTime gpxStat)
               ++ "\n" ++ (asciiArtHistogram 80 $ statSpeedHistogram gpxStat)

humanReadableDistance :: Double -> String
humanReadableDistance meters = (printf "%.2f" dist) ++ unit
  where (dist, unit) = if meters > 1000 then (meters / 1000, "km") else (meters, "m")

humanReadableDiffTime :: NominalDiffTime -> String
humanReadableDiffTime diffTime = (printf "%02d:%02d:%02d" hours minutes seconds)
  where abstime = (round $ abs diffTime)::Integer
        (hours, remSecs)   = abstime `divMod` 3600
        (minutes, seconds) = remSecs `divMod` 60

gpxStatistics gpx = GPXStatistics dist time hist
  where dist = totalDistance trip
        time = totalTime trip
        hist = mkSpeedHistogram trip
        trip = wholeTrip $ tracks gpx

-- concatenates all tracks and all segments to one big segment
wholeTrip :: [Track] -> [Point]
wholeTrip tracks = concatMap (\s -> points s) $ concatMap (\t -> segments t) tracks

mkSpeedHistogram :: [Point] -> SpeedHistogram
mkSpeedHistogram = foldl (\m k -> Map.insertWith (+) k 1 m) Map.empty . roundSpeeds . pointsToSpeeds

pointsToSpeeds :: [Point] -> [Maybe Double]
pointsToSpeeds [] = []
pointsToSpeeds points = reverse speeds
  where (_, speeds) = foldl (\(lastPoint, speeds) thisPoint -> (thisPoint, speed lastPoint thisPoint : speeds))
                            (head points, [])
                            points

-- convert from m/s to km/h and round to Int
roundSpeeds :: [Maybe Double] -> [Int]
roundSpeeds = map round . map (* 3.6) . catMaybes

maxValue :: Integral b => Map a b -> b
maxValue = Map.foldl max 0

asciiArtHistogram :: Int -> Map Int Int -> String
asciiArtHistogram width m = intercalate "\n" $ map oneLine $ filter (\(k, v) -> v > 1) assocs
  where maxVal = maxValue m
        assocs = Map.assocs m
        oneLine (k, v) = (printf "%02d" k) ++ " " ++ replicate (v * 80 `div` maxVal) '*'
