import Data.Either

l = const $ Left "foo"
r = const $ Right 2

n = const Nothing
j = const $ Just 1

foo :: Maybe Int
foo = do
  a <- j 2
  b <- n a
  c <- j b
  return c

-- bar :: Maybe Int
-- bar = do
  -- Right a <- j 1
  -- return a

baz :: IO ()
baz = do
  fail "bac"
  putStrLn "hi"

blah :: Maybe Int
blah = do
  a <- j 5
  c <- if a > 10 then Nothing else Just 3
  return c
