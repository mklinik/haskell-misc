module Environment where

import Data.Maybe

type Env = [(String, Int)]
newtype EnvM a = EnvM { runEnvM :: Env -> (a, Env) }

instance Monad EnvM where
  (EnvM f) >>= g  = EnvM (\s -> let (a, s_) = f s in runEnvM (g a) s_)
  return x = EnvM $ \s -> (x, s)

(>>>=) :: (Env -> (Int, Env)) -> (Int -> Env -> (Int, Env)) -> (Env -> (Int, Env))
f >>>= g = \s -> let (x, s1) = f s in g x s1

(>>>) :: (Env -> (a, Env)) -> (Env -> (b, Env)) -> (Env -> (b, Env))
f >>>  g = \s -> let (_, s1) = f s in g   s1

myReturn :: Int -> Env -> (Int, Env)
myReturn x = \s -> (x, s)

emptyEnv = []

assignM :: String -> Int -> EnvM ()
assignM name val = EnvM (\s -> ((), (name, val) : s))

readM :: String -> EnvM Int
readM name = EnvM (\s -> (fromMaybe 0 $ lookup name s, s))

assign :: String -> Int -> Env -> ((), Env)
assign name val env = ((), (name, val):env)

readOut :: String -> Env -> (Int, Env)
readOut name env = readOut_ name env env
  where
    readOut_ n [] env = (0, env)
    readOut_ n ((name,value):rest) env
      | n == name = (value, env)
      | otherwise = readOut_ n rest env

-- flup =
  -- assign "foo" 100 >>>
  -- assign "bar" 42 >>>
  -- readOut "foo" >>>= \x ->
  -- readOut "bar" >>>= \y ->
  -- myReturn (x+y)

foo = assign "foo" 100

bar = assign "foo" 100 >>> readOut "foo"
-- bar2 = assign "foo" 100 >> readOut "foo"

-- flap =
  -- assign "foo" 100 >>
  -- assign "bar" 42 >>
  -- readOut "foo" >>= \x ->
  -- readOut "bar" >>= \y ->
  -- return (x+y)


blah = do
  assignM "foo" 100
  assignM "bar" 42
  x <- readM "foo"
  y <- readM "bar"
  return (x+y)

run :: EnvM a -> Env -> a
run f e = fst $ (runEnvM f) e

runWithEnv :: Env -> (Env -> (a, Env)) -> a
runWithEnv e f = let (x, _) = f e in x

{-
  do
    assign "foo" 100
    x <- readOut "foo"
    assign "bar" (x + 1)
    readOut "bar"
-}

nothing = Nothing

test = do
  x <- nothing
  y <- nothing
  z <- nothing
  return (x, y, z)
