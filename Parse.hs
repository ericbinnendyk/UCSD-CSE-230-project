module Parse where

import FloatWithError

import Text.Parsec
import Text.Parsec.String

-- String parser from HW 2
parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p s = runParser p () "DUMMY" s

-- Parser for floats
floatP :: Parser Float
floatP = try withDecimal <|> withoutDecimal
  where
    withDecimal = do
        integerPart <- many digit
        _ <- char '.'
        decimalPart <- many digit
        return $ read (integerPart ++ "." ++ decimalPart)

    withoutDecimal = do
        integerPart <- many digit
        return $ read integerPart

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
opP :: Parser (Expression -> Expression -> Expression)
opP = constP "+" ExprAdd
    <|> constP "-" ExprSub
    <|> constP "*" ExprMul
    <|> constP "/" ExprDiv

exprP :: Parser Expression
exprP   = try (spaceP exprOpP)
   <|> parens (spaceP exprP)
   <|> (ExprNum <$> spaceP floatWithErrorP)

exprP' :: Parser Expression
exprP' = parens (spaceP exprP) <|> (ExprNum <$> spaceP floatWithErrorP)

exprOpP :: Parser Expression
exprOpP = do
   expr1 <- spaceP exprP'
   op <- opP
   expr2 <- spaceP exprP
   return (op expr1 expr2)

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
