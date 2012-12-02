module Main where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

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

  drawWindow <- widgetGetDrawWindow canvas
  onExpose canvas (\x -> do
    renderWithDrawable drawWindow myDraw
    return True)

  mainGUI

myDraw :: Render ()
myDraw = do
  setSourceRGB 0 0 0
  setLineWidth 5

  moveTo 10 10
  lineTo 20 20

  stroke
