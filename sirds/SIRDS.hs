module Main where

import Data.Char           -- for chr
import System.Random       -- for randomRIO
import qualified Data.Map as M
import Data.Map (Map(..))  -- make the datatype name available without
                           -- qualification
import System.IO           -- for file IO

-- --------------------------- --
-- Writing binary files        --
-- --------------------------- --

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile f x =
  do
    h <- openBinaryFile f WriteMode
    hPutStr h x
    hClose h

-- --------------------------- --
-- Type synonyms and datatypes --
-- --------------------------- --

type Color     = Int
data RGB       = RGB Color Color Color
                 deriving (Show, Eq)  -- makes it possible to use the show
                                      -- and (==) functions on an RGB
type Image     = [[RGB]]

data Link      = Linked Int Int | Unlinked Int
                 deriving (Show, Eq)  -- makes it possible to use the show
                                      -- and (==) functions on a Link
data Dir       = L | R
                 deriving (Show, Eq, Ord)  -- as above, plus ordering which
                                           -- is required for finite map
                                           -- keys
type Links     = Map (Int, Dir) Int

type Height    = Double
type HeightMap = [[Height]]

-- Some predefined colors.
red, green, blue :: RGB
red    = RGB 255 0 0
green  = RGB 0 255 0
blue   = RGB 0 0 255

gray :: Height -> RGB
gray c = let x = round (c * 255) in RGB x x x 

-- The classic tODO function.
tODO :: a -> a
tODO = id

-- ------------ --
-- (PPM) Images --
-- ------------ --

validColor :: Color -> Bool
validColor c = c >= 0 && c <= 255

validRGB :: RGB -> Bool
validRGB (RGB r g b) = validColor r && validColor g && validColor b

validImage :: Image -> Maybe (Int, Int)
validImage [] = Just (0, 0)
validImage rows =
  foldl validRow (Just (length $ head rows, 0)) rows
  where

    -- checks and counts rows
    validRow :: Maybe (Int, Int) -> [RGB] -> Maybe (Int, Int)
    validRow m pixels = do
      (expectedPixelCount, rowsSoFar) <- m
      pixelCount <- foldl validPixel (Just 0) pixels
      if pixelCount == expectedPixelCount
        then (^-^) (expectedPixelCount, rowsSoFar + 1)
        else (>.<)

    -- checks and counts pixels
    validPixel :: Maybe Int -> RGB -> Maybe Int
    validPixel m rgb = m >>= (\num -> if validRGB rgb then (^-^) (num+1) else (>.<) )

ppmHeader :: (Int,Int) -> String
ppmHeader (x, y) = "P6 " ++ (show x) ++ " " ++ (show y) ++ " 255\n"

encodeRGB :: RGB -> String
encodeRGB (RGB r g b) = [chr r, chr g, chr b]

ppmData :: Image -> String
ppmData image = concatMap (concatMap encodeRGB) image

writePPM :: FilePath -> Image -> IO ()
writePPM path image =
  case validImage image of
    Nothing -> putStrLn "writePPM: invalid image"
    Just imageDimensions -> writeBinaryFile path $ (ppmHeader imageDimensions) ++ (ppmData image)

-- The following functions are utilities for image computation.
-- They're not relevant for the assignment, only if you want to
-- define your own images as Haskell functions.

-- type of image generators
type Gen a = (Int, Int) -> a

-- turns an image generator into a list-of-list image;
-- kept polymorphic so that it works for HeightMap and
-- Image
toImage :: (Int,Int) -> (Gen a) -> [[a]]
toImage (maxX,maxY) f = [ [ f (x,y) | x <- [0..maxX-1] ] | y <- [0..maxY-1] ]

-- composes two image-generating functions, retaining the maximal values
(\^/) :: Ord a => Gen a -> Gen a -> Gen a
(f \^/ g) (x,y) = f (x,y) `max` g (x,y)

-- translate an image
translate :: (Int,Int) -> Gen a -> Gen a
translate (dx,dy) f (x,y) = f (x - dx, y - dy)

-- z-scale a heightmap generator
zscale :: Double -> Gen Double -> Gen Double
zscale s f (x,y) = s * f (x,y)

-- Generates Figure 2.
chess :: Image
chess = toImage (80,80) (gray . chess' 10)

chess' :: Int -> Gen Height
chess' n (x,y) = fromIntegral (((x `div` n) + (y `div` n) + 1) `mod` 2)

-- Generates Figure 3.
gradient :: Image
gradient = toImage (80,80)
           (\ (x,y) -> let step = round (fromIntegral (x + y) * 255 / 158)
                       in  RGB step step 255)

-- Generates Figure 4.
circular = toImage (80,80)
           (\ (x,y) -> let dist = round (sqrt (fromIntegral ((x - 40)^2 + (y - 40)^2))
                                           * 255 / 57)
                       in  RGB dist (255 - dist) 255)

-- ----- --
-- Links --
-- ----- --

-- The function validLink checks if a given Link is valid, i.e.,
-- if the invariant holds: a valid links is either an Unlinked
-- point or it is a Linked pair of points where the left point
-- is strictly smaller than the right point. This function is
-- given.
validLink :: Link -> Bool
validLink (Linked x y) = x < y
validLink (Unlinked _) = True

-- The `better' operator for links. Use pattern matching, recall
-- the standard recipe!
(>%>) :: Link -> Link -> Bool
Unlinked _   >%> _          = False
_            >%> Unlinked _ = True
Linked l1 r1 >%> Linked l2 r2 = (r1 - l1) < (r2 - l2)

-- The Links structure with no links is just the empty finite map.
-- This function is given.
noLinks :: Links
noLinks = M.empty

add :: Link -> Links -> Links
add (Unlinked _) links = links
add (Linked l r) links =
  M.insert (r, R) l $ M.insert (l, L) r links

del :: Link -> Links -> Links
del (Unlinked _) links = links
del (Linked l r) links =
  M.delete (r, R) $ M.delete (l, L) links

query :: Link -> Dir -> Links -> Link
query (Unlinked p) dir links = lookupLink p dir links
query (Linked _ r) L links = lookupLink r L links
query (Linked l _) R links = lookupLink l R links

lookupLink :: Int -> Dir -> Links -> Link
lookupLink p dir links = case M.lookup (p, dir) links of
  Just p_ -> mkLink p p_
    where
      mkLink l r
        | l < r = Linked l r
        | otherwise = Linked r l
  Nothing -> Unlinked p

link :: Link -> Links -> Links
link (Unlinked _) links = links
link l links
  | (l >%> left) && (l >%> right) = add l $ del right $ del left links
  | otherwise = links
    where
      left = query l L links
      right = query l R links

-- --------------------- --
-- Stereogram generation --
-- --------------------- --

-- The following constants are required for calculating
-- the separation. You usually should not need to change
-- them. If you do, be careful because unreasonable values
-- can lead to strange results.

dpi :: Double
dpi = 72.0         -- typical screen resolution (pixels per inch)

e :: Double
e = 2.5 * dpi      -- eye distance in pixels

d :: Double
d = 3.0            -- "distance" between projection plane and base
                   -- plane of the 3D image

separation :: Double -> Int
separation z = tODO 0

sirdsLine :: [Height] -> Links
sirdsLine hs = tODO noLinks

-- Assign random colors to the points of a line, but respect
-- the computed links: linked points should get the same color.
-- This function is given.
assign :: Int -> Links -> IO [RGB]
assign maxX cs =
  do
    let xs      = [0 .. maxX - 1]            -- all relevant x-coordinate
    let classes = map (findRightmost cs) xs  -- equivalence classes of colors
    -- compute random colors
    colorsR <- mapM randomRIO (replicate maxX (0,255))
    colorsG <- mapM randomRIO (replicate maxX (0,255))
    colorsB <- mapM randomRIO (replicate maxX (0,255))
    let colors  = zipWith3 RGB colorsR colorsG colorsB
    return (map (colors !!) classes)

-- Links can form chains in the Links data structure. For a given
-- x-coordinate, the function findRightmost finds the rightmost
-- point in a chain of links. If all the points in a chain of links
-- get the same color as the rightmost point in that chain, then
-- in particular all linked points end up with the same color.
-- This function is given.
findRightmost :: Links -> Int -> Int
findRightmost cs x =
  case query (Unlinked x) R cs of
    Unlinked x' -> x'
    Linked _ x' -> findRightmost cs x'

-- The function sirds computes a SIRDS from a heightmap.
-- It processes the input data line by line using sirdsLine,
-- and the assigns colors using the assign function.
sirds :: HeightMap -> IO Image
sirds = mapM (\ line -> assign (length (line)) (sirdsLine line))

-- -------------------- --
-- Decoding stereograms --
-- -------------------- --

decode :: Image -> Image
decode = map decodeLine

decodeLine :: [RGB] -> [RGB]
decodeLine ps = map (\ x -> M.findWithDefault red x (M.map gray (decodeLine' 0 M.empty ps)))
                    [0 .. length ps - 1]

decodeLine' :: Int -> Map Int Height -> [RGB] -> Map Int Height
decodeLine' _ acc []     = acc
decodeLine' x acc (p:ps) =
  let range = drop (separation 1 - 1)                -- separation 1 is the minimum
                (take (separation 0) (zip [1..] ps)) -- separation 0 is the maximum
      candidates = [ x | (x,q) <- range, p == q ]
      acc' = case candidates of
               []     -> acc
               (x':_) -> M.insert (x + x' `div` 2) (invSeparation x') acc
  in  decodeLine' (x + 1) acc' ps

-- The (approximate) inverse of the separation function.
invSeparation :: Int -> Double
invSeparation s = (2 * d * fromIntegral s - d * e) / (fromIntegral s - e)


-- --------------------------- --
-- Sample heightmap generators --
-- --------------------------- --

-- Turns a heightmap into an image, mapping the heights to gray values.
heightMap :: HeightMap -> Image
heightMap = map (map gray)

doubleChess :: HeightMap
doubleChess = toImage (maxX, maxY) doubleChess'

doubleChess' :: Gen Double
doubleChess' = zscale 0.8 (translate (25,25) (chess' 100)) \^/ chess' 50



-- ------------ --
-- Main program --
-- ------------ --

-- Default resolution to use; larger, especially wider, images are easier
-- to view, but of course, also more space- and time-intensive to generate.
maxX = 800
maxY = 400

-- An example main program. Feel free to change it to print other images
-- or perform other computations.
main =
  do
    writePPM "doubleChessPattern.ppm" (heightMap doubleChess) -- prints the pattern, unencoded
    i <- sirds doubleChess 
    writePPM "doubleChess.ppm" i -- prints encoded chess pattern SIRDS
    writePPM "doubleChessDecoded.ppm" (decode i) -- prints decoded chess pattern SIRDS


(^-^) = Just
(>.<) = Nothing
