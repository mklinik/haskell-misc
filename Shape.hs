{-# LANGUAGE ExistentialQuantification #-}
module Shapes where

class IShape a where
  prompt :: a -> String

data Shape = forall a . IShape a => Shape a

data Circle = Circle Int Int Int

data Smiley = Smiley Circle

data Rectangle = Rectangle Int Int Int Int

instance IShape Circle where
  prompt (Circle x y r) = "circle at " ++ show (x,y) ++ " with radius " ++ show r

instance IShape Smiley where
  prompt (Smiley _) = ":)"

instance IShape Rectangle where
  prompt (Rectangle x y w h) = "rectangle at " ++ show (x,y) ++ " with dimensions " ++ show (w,h)

instance IShape Shape where
  prompt (Shape s) = prompt s

circle x y r = Shape $ Circle x y r
smiley = Shape $ Smiley $ Circle 2 2 2
rectangle x y w h = Shape $ Rectangle x y w h

inputs :: [Int] -> IO [Int]
inputs is = do
  i <- read <$> getLine
  case i of
    0 -> return is
    _ -> inputs (is ++ [i])

mkShape :: Int -> Shape
mkShape i = case i of
  1 -> circle 1 1 1
  2 -> smiley
  3 -> rectangle 1 2 3 4

main = do
  shapes <- map mkShape <$> inputs []
  mapM_ (putStrLn . prompt) shapes
