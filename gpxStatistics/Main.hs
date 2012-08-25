module Main where

import           Util
import           System.Environment (getArgs)
import           System.Console.GetOpt

data Options = Options
  { mode :: (FilePath -> IO ())
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { mode = printGPXStatistics
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["histogram"]
      (NoArg (\o -> o { mode = printGPXStatistics }))
      "print an ASCII histogram (default)"
  , Option ['s'] ["speed"]
      (NoArg (\o -> o { mode = printGPXSpeeds }))
      "print a list of speed values"
  ]

mkOptions :: [String] -> IO (Options, [String])
mkOptions argv =
  case getOpt Permute options argv of
    (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "gpxStatistics [OPTION...] files..."

main :: IO ()
main = do
  (opts, args) <- getArgs >>= mkOptions
  mapM_ (mode opts) args
