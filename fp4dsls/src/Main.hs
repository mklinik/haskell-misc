-- Jeremy Gibbons, 2015, Functional Programming for Domain-Specific Languages
module Main where

import qualified SetLangDeep as D
import qualified SetADT as A
import qualified SetLangShallow as S

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

deep    = ((((D.Empty `D.Insert` 3) `D.Insert` 2) `D.Delete` 3) `D.Insert` 1) `D.member` 3
adt     = ((((A.empty `A.insert` 3) `A.insert` 2) `A.delete` 3) `A.insert` 1) `A.member` 3
shallow = ((((S.empty `S.insert` 3) `S.insert` 2) `S.delete` 3) `S.insert` 1) `S.member` 3

deepSet = ((((D.Empty `D.Insert` 3) `D.Insert` 2) `D.Delete` 3) `D.Insert` 1)

main :: IO ()
main = do
  print deep
  print adt
  print shallow
  print $ (S.membership $ D.elements deepSet) `S.member` 3
  interpret "{}+3+2-3+1?3"
  interpret "{}?3"
  interpret "{}+3?3"

interpret :: String -> IO ()
interpret input =
  case runParser pProgram () "-" input of
    Left err -> print err
    Right b -> print b

pProgram :: Parser Bool
pProgram = D.member <$> pIntegerSet <* char '?' <*> pInteger

pIntegerSet :: Parser D.IntegerSet
pIntegerSet = foldl (flip id) D.Empty <$ char '{' <* char '}' <*> many pSetOp

pSetOp :: Parser (D.IntegerSet -> D.IntegerSet)
pSetOp = (flip D.Insert) <$ char '+' <*> pInteger
     <|> (flip D.Delete) <$ char '-' <*> pInteger

pInteger :: Parser Integer
pInteger = integer haskell
