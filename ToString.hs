{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

import Data.Maybe
import Text.Read
import qualified Text.ParserCombinators.ReadP as P

class ToString a where
  toString :: a -> String

readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    [] -> Left "Prelude.read: no parse"
    _ -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x



instance Show a => ToString a where
  toString a = let s = show a in fromMaybe s (readMaybe s)

(##) :: (ToString a, ToString b) => a -> b -> String
a ## b = toString a ++ toString b

name = "Marvin"
age :: Int
age = 23
profession = "\955-scientist"

foo = name##" the "##age##"-year-old "##profession
bar = "Hello, "##name
baz = age##" stinks"
