module Classes where

class Foo a where
  foo :: a -> Int

instance Foo Int where
  foo = const 10

newtype Int2 = Int2 Int

instance Foo Int2 where
  foo = const 20
