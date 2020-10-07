module MyFunctor where

import Prelude hiding ((>>=))

class MyFunctor f where
  fmap :: (a -> b) -> f a -> f b

instance MyFunctor (Either e) where
  fmap _ (Left e) = Left e
  fmap g (Right a) = Right (g a)

instance MyFunctor ((,) e) where
  fmap g (e, a) = (e, g a)

instance MyFunctor ((->) e) where
  fmap g f = g . f


class Functor f => Pointed f where
  pure :: a -> f a

instance Pointed Maybe where
  pure = Just

instance Pointed [] where
  pure x = [x]

instance Pointed ((->) e) where
  pure = const


class Pointed f => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  (Just f) <*> (Just a) = Just (f a)
  _ <*> _ = Nothing

instance Applicative [] where
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative ((->) e) where
  f <*> g = \e -> f e $ g e


class Applicative m => MyMonad m where
  (>>=) :: m a -> (a -> m b) -> m b

instance MyMonad Maybe where
  (Just a) >>= f = f a
  _ >>= _ = Nothing

instance MyMonad [] where
  xs >>= f = concat [f x | x <- xs]

instance MyMonad ((->) e) where
  -- (e -> a) >>= (a -> (e -> b)) -> (e -> b)
  g >>= f = \e -> f (g e) e
