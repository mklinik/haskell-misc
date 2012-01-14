import Data.Fixed (mod')
import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.Random as Random
import Control.Monad
import System.IO

data RGB = RGB Int Int Int

pi2 = pi * 2

kickedRotator k (pn, xn) = (pn, xn) : (kickedRotator k (pnp1 `mod'` pi2, xnp1 `mod'` pi2))
  where
    pnp1 = pn + k * (sin xn)
    xnp1 = (xn + pnp1)

-- take one data point in the range (-2*pi, 2*pi)
-- and scale it to the image with and height
scale :: Int -> Int -> (Double, Double) -> (Int, Int)
scale width height (p, x) = (pScaled, xScaled)
  where
    pScaled = floor $ p * (fromIntegral width) / pi2
    xScaled = floor $ x * (fromIntegral height) / pi2

printRGB (RGB r g b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " "

printPixel :: Map (Int, Int) RGB -> Int -> Int -> String
printPixel m x y = case Map.lookup (x, y) m of
  Nothing -> printRGB $ RGB 255 255 255
  Just rgb -> printRGB rgb

map2ppm :: Map (Int, Int) RGB -> Int -> Int -> String
map2ppm m width height =
  "P3\n" ++
  (show width) ++ " " ++ (show height) ++ "\n" ++
  "255\n" ++
  (foldr (++) "" [ printPixel m x y | x <- [0..(width - 1)], y <- [0..(height - 1)] ])

-- an infinite list of random colors
randomColor :: IO RGB
randomColor = do
  r <- liftM (\x -> x `mod` 256) Random.randomIO
  g <- liftM (\x -> x `mod` 256) Random.randomIO
  b <- liftM (\x -> x `mod` 256) Random.randomIO
  return $ RGB r g b

randomPoint :: IO (Double, Double)
randomPoint = do
  x <- liftM (\x -> x * pi2) Random.randomIO
  y <- liftM (\x -> x * pi2) Random.randomIO
  return (x, y)


insertPixels :: Int -> RGB -> Map (Int, Int) RGB -> [(Int, Int)] -> Map (Int, Int) RGB
insertPixels iterations color m pixels = foldl (\map pixel -> Map.insert pixel color map) m (take iterations $ pixels)

main = do
  let iterations = 10000
  let k = 0.971635
  let width = 600
  let height = 600
  startPoints <- replicateM 6 randomPoint
  hPutStr stderr $ show startPoints
  let lines = map (map (scale width height)) $ map (kickedRotator k) startPoints
  foo <- foldM (\m pixels -> do
                   color <- randomColor
                   return $ insertPixels iterations color m pixels
                  ) Map.empty lines
  putStr $ map2ppm foo width height
  putStr ""
