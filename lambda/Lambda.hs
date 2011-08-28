module Lambda where

data Term
  = Var Int
  | App Term Term
  | Lam Term

foldTerm :: (Int -> r) -> (r -> r -> r) -> (r -> r) -> Term -> r
foldTerm var _   _   (Var n)   = var n
foldTerm val app lam (App l r) = app (foldTerm val app lam l) (foldTerm val app lam r)
foldTerm val app lam (Lam t)   = lam (foldTerm val app lam t)

instance Show Term where
  show = foldTerm show (\x y -> "(" ++ x ++ " " ++ y ++ ")") (\t -> "\\.(" ++ t ++ ")")




-- some examples
t1 = Var 0

-- identity
i = Lam $ Var 0

-- constant
c = Lam $ Lam $ Var 1

-- Y
y = App (Lam $ Lam $ (Var 1) `App` (Var 0) `App` (Var 0))
        (Lam $ Lam $ (Var 1) `App` (Var 0) `App` (Var 0))

-- church numerals
zero  = Lam $ Lam $ Var 0
one   = Lam $ Lam $ (Var 1) `App` (Var 0)
two   = Lam $ Lam $ (Var 1) `App` ((Var 1) `App` (Var 0))
three = Lam $ Lam $ (Var 1) `App` ((Var 1) `App` ((Var 1) `App` (Var 0)))

int2church :: Int -> Term
int2church n = Lam $ Lam $ int2church_ n $ Var 0
int2church_ 0 t = t
int2church_ n t = (Var 1) `App` (int2church_ (n - 1) t)

church2int :: Term -> Maybe Int
church2int (Lam (Lam t)) = church2int_ t
church2int _ = Nothing

church2int_ :: Term -> Maybe Int
church2int_ (Var 0) = Just 0
church2int_ (App (Var 1) t) = do
  n <- church2int_ t
  return (n + 1)
church2int_ _ = Nothing
