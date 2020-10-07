{-# LANGUAGE GADTs, KindSignatures #-}
module SetLangDeep (IntegerSet(Empty, Insert, Delete), member, elements) where

import qualified SetADT as A

-- Gibbons says: we use Haskell's GADTs to emphasize the return types, even
-- though we make no use of the extra expressive power
data IntegerSet :: * where
  Empty  ::                          IntegerSet
  Insert :: IntegerSet -> Integer -> IntegerSet
  Delete :: IntegerSet -> Integer -> IntegerSet

member :: IntegerSet -> Integer -> Bool
member Empty _ = False
member (Insert xs x) y = (x == y) || (member xs y)
member (Delete xs x) y = (x /= y) && (member xs y)

elements :: IntegerSet -> A.IntegerSet
elements Empty = A.empty
elements (Insert xs x) = A.insert (elements xs) x
elements (Delete xs x) = A.delete (elements xs) x
