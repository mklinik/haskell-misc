import Data.Fixed (mod')
import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.Random as Random
import Control.Monad

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

printRGB (RGB r g b) = do
  putStr (show r)
  putStr " "
  putStr (show g)
  putStr " "
  putStr (show b)
  putStr " "

printPixel :: Map (Int, Int) RGB -> Int -> Int -> IO ()
printPixel m x y =
  case Map.lookup (x, y) m of
    Nothing -> printRGB $ RGB 255 255 255
    Just rgb -> printRGB rgb

printPPM :: Map (Int, Int) RGB -> Int -> Int -> IO ()
printPPM m width height = do
  putStrLn "P3"
  putStrLn $ (show width) ++ " " ++ (show height)
  putStrLn "255"
  sequence_ [ printPixel m x y | x <- [0..(width - 1)], y <- [0..(height - 1)] ]

-- an infinite list of random colors
randomColors :: [IO RGB]
randomColors =
  (do r <- liftM (\x -> x `mod` 256) Random.randomIO
      g <- liftM (\x -> x `mod` 256) Random.randomIO
      b <- liftM (\x -> x `mod` 256) Random.randomIO
      return $ RGB r g b
  ) : randomColors

insertPixels :: Int -> RGB -> Map (Int, Int) RGB -> [(Int, Int)] -> Map (Int, Int) RGB
insertPixels iterations color m pixels = foldl (\map pixel -> Map.insert pixel color map) m (take iterations $ pixels)

main = do
  let iterations = 20000
  let k = 0.971635
  let width = 300
  let height = 300
  let startPoints = [(1,1), (2,2), (3,3), (4,4)]
  let lines = map (map (scale width height)) $ map (kickedRotator k) startPoints
  let foo = foldl (insertPixels iterations (RGB 0 0 255)) Map.empty lines
  printPPM foo width height
