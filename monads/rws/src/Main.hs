module Main where

import Control.Monad.RWS

-- let's just mess around a bit
-- read from options
data Options = Option1 | Option2

myComputation :: RWS Options String () ()
myComputation = do
  opt <- ask
  case opt of
    Option1 -> tell "foo"
    Option2 -> tell "bar"
  tell "baz"

instance Monoid Int where
  mempty = 0
  mappend x y = x + 10 * y

myComputation2 :: RWS Options Int () ()
myComputation2 = do
  opt <- ask
  case opt of
    Option1 -> tell 1
    Option2 -> tell 2
  tell 1
  tell 7
  tell 8
  tell 9

main :: IO ()
main = do
  let (_, w) = evalRWS myComputation2 Option2 ()
  print w
