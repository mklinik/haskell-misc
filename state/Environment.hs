module Environment where

type Env = [(String, Int)]

emptyEnv = []

assign :: String -> Int -> Env -> ((), Env)
assign name val env = ((), (name, val):env)

readOut :: String -> Env -> (Int, Env)
readOut name env = readOut_ name env env
  where
    readOut_ n [] env = (0, env)
    readOut_ n ((name,value):rest) env
      | n == name = (value, env)
      | otherwise = readOut_ n rest env

flup =
  assign "foo" 100 >>>
  assign "bar" 42 >>>
  readOut "foo" >>>= \x ->
  readOut "bar" >>>= \y ->
  myReturn (x+y)

foo = assign "foo" 100

bar = assign "foo" 100 >>> readOut "foo"
-- bar2 = assign "foo" 100 >> readOut "foo"

-- flap =
  -- assign "foo" 100 >>
  -- assign "bar" 42 >>
  -- readOut "foo" >>= \x ->
  -- readOut "bar" >>= \y ->
  -- return (x+y)


-- blah = do
  -- assign "foo" 100
  -- assign "bar" 42
  -- x <- readOut "foo"
  -- y <- readOut "bar"
  -- return (x+y)

(>>>=) :: (Env -> (Int, Env)) -> (Int -> Env -> (Int, Env)) -> (Env -> (Int, Env))
f >>>= g = \s -> let (x, s1) = f s in g x s1

(>>>) :: (Env -> (a, Env)) -> (Env -> (b, Env)) -> (Env -> (b, Env))
f >>>  g = \s -> let (_, s1) = f s in g   s1

myReturn :: Int -> Env -> (Int, Env)
myReturn x = \s -> (x, s)

-- instance Monad ??? where
  -- (>>=) = (>>>=)
  -- (>>) = (>>>)
  -- return = myReturn


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
