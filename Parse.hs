module Parse where

import FloatWithError

import Text.Parsec
import Text.Parsec.String

-- String parser from HW 2
parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p s = runParser p () "DUMMY" s

-- Parser for floats
floatP :: Parser Float
floatP = do
    integerPart <- many digit
    _ <- string "."
    decimalPart <- many digit
    return $ read (integerPart ++ "." ++ decimalPart)

-- >>> parseFromString floatWithErrorP "4.2 +/- 2.3"
-- Right (FWE 4.2 2.3)

-- Parses floats with error bars
floatWithErrorP :: Parser FloatWithError
floatWithErrorP = do
    low <- spaceP floatP
    _ <- char '+'
    _ <- char '/'
    _ <- char '-'
    high <- spaceP floatP
    return $ FWE low high

constP :: String -> a -> Parser a
constP s x = do
   string s
   return x

-- Currently this only works for operations on floats. TODO: Make this work with expressions instead. Maybe new operations for expressions?
opP :: Parser (FloatWithError -> FloatWithError -> FloatWithError)
opP = constP "+" add_fwe
    <|> constP "-" sub_fwe
    <|> constP "*" mul_fwe
    <|> constP "/" div_fwe

{- TODO: expression parser
exprP :: Parser Expression
exprP =
  try (spaceP exprOpP)
    <|> parens (spaceP exprP)
    <|> (ExprNum <$> spaceP floatWithErrorP)

exprP' :: Parser Expression
exprP' = parens (spaceP exprP) <|>

exprOpP :: Parser Expression
exprOpP = do
  expr1 <- spaceP exprP'
  op <- opP
  expr2 <- spaceP exprP
  return (op expr1 expr2)
-}

spaceP :: Parser a -> Parser a
spaceP p = do
   _ <- spaces
   x <- p
   _ <- spaces
   return x

parens :: Parser a -> Parser a
parens aP = do
    _ <- char '('
    a <- spaceP aP
    _ <- char ')'
    return a