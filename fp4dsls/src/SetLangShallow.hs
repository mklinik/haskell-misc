module SetLangShallow (IntegerSet, empty, insert, delete, member, membership) where

import qualified SetADT as A

newtype IntegerSet = IS (Integer -> Bool)

empty :: IntegerSet
empty = IS (const False)

insert :: IntegerSet -> Integer -> IntegerSet
insert (IS f) x = IS (\y -> (x == y) || f y)

delete :: IntegerSet -> Integer -> IntegerSet
delete (IS f) x = IS (\y -> (x /= y) && f y)

member :: IntegerSet -> Integer -> Bool
member (IS f) y = f y

membership :: A.IntegerSet -> IntegerSet
membership xs = IS (\y -> A.member xs y)
