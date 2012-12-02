module Main where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import System.Random

hello :: (ButtonClass b) => b -> IO ()
hello b = set b [buttonLabel := "Hello World"]

setup = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit
  return window

main = do
  window <- setup

  frame <- frameNew
  containerAdd window frame

  canvas <- drawingAreaNew
  containerAdd frame canvas

  widgetShowAll window

  gen <- getStdGen

  drawWindow <- widgetGetDrawWindow canvas
  onExpose canvas (\x -> do
    renderWithDrawable drawWindow (myDraw gen)
    return True)

  mainGUI

myDraw :: RandomGen g => g -> Render ()
myDraw gen = do
  setLineWidth 2

  sequence $ concat [ [ moveTo (scale x) (scale y)
                      , (liftIO randomIO) >>= \rand -> lineTo (scale (x+(direction rand))) (scale (y+1))
                      ]
    | y <- [0..dimY], x <- [0..dimX] ]

  stroke

  where
    dimX = 30
    dimY = 30
    scale = (*15)
    direction :: Int -> Double
    direction x = (if x < 0 then (-1.0) else 1.0)
