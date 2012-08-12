module Main where

import           Data.Time.Clock
import           System.Locale
import           Geo.GPX.Conduit
import           Geo.Computations
import           Text.Printf
import           System.Environment (getArgs)

main = getArgs >>= mapM_ printGPXStatistics

printGPXStatistics :: FilePath -> IO ()
printGPXStatistics file = do
  g <- readGPXFile file
  case g of
    Just gpx -> print $ gpxStatistics gpx
    Nothing  -> putStrLn $ "error reading gpx file " ++ file

data GPXStatistics = GPXStatistics
  { statTotalDistance  :: Double -- in meters
  , statTotalTime      :: NominalDiffTime
  }

instance Show GPXStatistics where
  show gpxStat = "total distance: " ++ (humanReadableDistance $ statTotalDistance gpxStat)
               ++ "\ntotal time: " ++ (humanReadableDiffTime $ statTotalTime gpxStat)

humanReadableDistance :: Double -> String
humanReadableDistance meters = (printf "%.2f" dist) ++ unit
  where (dist, unit) = if meters > 1000 then (meters / 1000, "km") else (meters, "m")

humanReadableDiffTime :: NominalDiffTime -> String
humanReadableDiffTime diffTime = (printf "%02d:%02d:%02d" hours minutes seconds)
  where abstime = (round $ abs diffTime)::Integer
        (hours, remSecs)   = abstime `divMod` 3600
        (minutes, seconds) = remSecs `divMod` 60

gpxStatistics gpx = GPXStatistics dist time
  where dist = totalDistance trip
        time = totalTime trip
        trip = wholeTrip $ tracks gpx

-- concatenates all tracks and all segments to one big segment
wholeTrip :: [Track] -> [Point]
wholeTrip tracks = concatMap (\s -> points s) $ concatMap (\t -> segments t) tracks
