import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad.IO.Class

main = do
  x <- generate arbitrary
  print $ x + (1::Int)
