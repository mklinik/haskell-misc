module Main where

import Graphics.UI.Gtk

hello :: (ButtonClass b) => b -> IO ()
hello b = set b [buttonLabel := "Hello World"]

setup = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit
  return window

run window = do
  widgetShowAll window
  mainGUI

main = do
  window <- setup

  run window
