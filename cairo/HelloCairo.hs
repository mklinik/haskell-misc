module Main where

import Graphics.UI.Gtk

main = do
  initGUI
  window <- windowNew
  widgetShowAll window
  mainGUI
