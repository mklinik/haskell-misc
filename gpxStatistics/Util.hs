module Util where

import           Data.Time.Clock
import           Geo.GPX.Conduit
import           Geo.Computations
import           Text.Printf
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.List (intercalate)

gpxInteract :: Show a => (GPX -> a) -> FilePath -> IO ()
gpxInteract f file = do
  g <- readGPXFile file
  case g of
    Just gpx -> print $ f gpx
    Nothing  -> putStrLn $ "error reading gpx file " ++ file

printGPXStatistics :: FilePath -> IO ()
printGPXStatistics = gpxInteract gpxStatistics

printGPXSpeeds :: FilePath -> IO ()
printGPXSpeeds = gpxInteract gpxSpeeds

printGPXHistogram :: FilePath -> IO ()
printGPXHistogram = gpxInteract gpxHistogram

type SpeedHistogram = Map Int Int

newtype GPXSpeeds = GPXSpeeds [Double]

instance Show GPXSpeeds where
  show (GPXSpeeds []) = ""
  show (GPXSpeeds (s:speeds)) = (show s) ++ "\n" ++ (show $ GPXSpeeds speeds)

gpxSpeeds :: GPX -> GPXSpeeds
gpxSpeeds gpx = GPXSpeeds speeds
  where
    trip = concatTracks $ tracks gpx
    speeds = (map (* 3.6) . catMaybes . pointsToSpeeds) trip

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

gpxStatistics :: GPX -> GPXStatistics
gpxStatistics gpx = GPXStatistics dist time hist
  where dist = totalDistance trip
        time = totalTime trip
        hist = mkSpeedHistogram trip
        trip = concatTracks $ tracks gpx

newtype ShowHistogram = ShowHistogram SpeedHistogram

instance Show ShowHistogram where
  show (ShowHistogram h) = intercalate "\n" [ show speed ++ " " ++ show number | (speed, number) <- Map.assocs h]

gpxHistogram :: GPX -> ShowHistogram
gpxHistogram gpx = ShowHistogram $ mkSpeedHistogram trip
  where
    trip = concatTracks $ tracks gpx

-- concatenates all tracks and all segments to one big segment
concatTracks :: [Track] -> [Point]
concatTracks = concatMap (\s -> points s) . concatMap (\t -> segments t)

mkSpeedHistogram :: [Point] -> SpeedHistogram
mkSpeedHistogram = foldl (\m k -> Map.insertWith (+) k 1 m) Map.empty . roundSpeeds . pointsToSpeeds

pointsToSpeeds :: [Point] -> [Maybe Double]
pointsToSpeeds [] = []
pointsToSpeeds pts = reverse speeds
  where (_, speeds) = foldl (\(lastPoint, soFar) thisPoint -> (thisPoint, speed lastPoint thisPoint : soFar))
                            (head pts, [])
                            pts

-- convert from m/s to km/h and round to Int
roundSpeeds :: [Maybe Double] -> [Int]
roundSpeeds = map round . map (* 3.6) . catMaybes

maxValue :: Integral b => Map a b -> b
maxValue = Map.foldl max 0

asciiArtHistogram :: Int -> Map Int Int -> String
asciiArtHistogram width m = intercalate "\n" $ map oneLine $ filter (\(_, v) -> v > 1) assocs
  where maxVal = maxValue m
        assocs = Map.assocs m
        oneLine (k, v) = (printf "%02d" k) ++ " " ++ replicate (v * width `div` maxVal) '*'
