module Shape2 where

data Shape = Shape
  { _prompt :: String
  , _translate :: Int -> Int -> Shape
  }

circle :: Int -> Int -> Int -> Shape
circle x y r = Shape prompt translate
  where
    prompt = "circle at " ++ show (x,y) ++ " with radius " ++ show r
    translate dx dy = circle (x+dx) (y+dy) r

smiley :: Shape
smiley = smiley_ (circle 2 2 2) prompt translate
  where
    smiley_ c prompt translate = Shape prompt translate
      where
        prompt = ":)" ++ (_prompt c)
        translate dx dy = smiley_ (translate c dx dy) 

rectangle x y w h = Shape prompt translate
  where
    prompt = "rectangle at " ++ show (x,y) ++ " with dimensions " ++ show (w,h)
    translate dx dy = rectangle (x+dx) (y+dy) w h

inputs :: [Int] -> IO [Int]
inputs is = do
  i <- read <$> getLine
  case i of
    0 -> return is
    _ -> inputs (is ++ [i])

mkShape :: Int -> Shape
mkShape i = case i of
  1 -> _translate (circle 1 1 1) 7 7
  2 -> smiley
  3 -> rectangle 1 2 3 4

main = do
  shapes <- map mkShape <$> inputs []
  mapM_ (putStrLn . _prompt) shapes
