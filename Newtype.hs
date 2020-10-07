module Newtype where

newtype Foo = Foo String
data Bar = Bar String

unFoo :: Foo -> String
unFoo (Foo s) = s

unBar :: Bar -> String
unBar (Bar s) = s

main = do
  putStrLn "hello world"
  putStrLn $ unBar $ Bar $ "hello bar"
  -- In Haskell, newtype constructors are first-class functions
  -- In Clean, they are not
  putStrLn $ unFoo $ Foo $ "hello foo"
