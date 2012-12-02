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
  setLineWidth 3
  setLineCap LineCapRound

  sequence [ liftIO randomIO >>= randomBox x y | y <- [0..dimY], x <- [0..dimX] ]

  stroke

  where
    dimX = 40
    dimY = 40

    scale = (*15)
    randomBox x y rand = (if rand then boxA else boxB) x y
    boxA x y = sequence [ moveTo (scale (x+1)) (scale y), lineTo (scale (x))   (scale (y+1)) ]
    boxB x y = sequence [ moveTo (scale x)     (scale y), lineTo (scale (x+1)) (scale (y+1)) ]
