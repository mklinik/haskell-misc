module Printf where

-- This is essentially a shallow embedding of a DSL. Deep embedding is also
-- possible, see Ralf Hinze's paper http://okmij.org/ftp/typed-formatting/
-- I know that I found this implementation in some paper, but forgot which one
-- it was.

int :: (String -> a) -> String -> Int -> a
int cont str n = cont (str ++ show n)

lit :: String -> (String -> a) -> String -> a
lit l cont str = cont (str ++ l)

str :: (String -> a) -> String -> String -> a
str cont str s = cont (str ++ s)

eol cont str = cont (str ++ "\n")

format :: ((String -> String) -> String -> a) -> a
format f = f id ""

name = "Marvin"
age = 27 :: Int
occupation = "\955-scientist"

myFormat = (str . lit " the " . int . lit "-year-old " . str . eol)

main = putStr $ format myFormat name age occupation
