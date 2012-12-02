module Main where

import Graphics.UI.Gtk

hello :: (ButtonClass b) => b -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main = do
  initGUI
  window <- windowNew
  button <- buttonNew
  set window [containerChild := button, containerBorderWidth := 10]
  onClicked button (hello button)
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
