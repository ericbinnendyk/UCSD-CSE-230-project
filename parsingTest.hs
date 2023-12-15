-- datatype representing a single token
-- includes things like IntToken 5 and OpToken '+'
data Token
  = IntToken Int
  | OpToken Char
  | OpenPToken -- open parenthesis
  | ClosePToken -- close parenthesis
  | NullToken
  deriving (Show)

-- so far, this only parses single-digit numbers as tokens
parse :: String -> (Token, String)
parse (x:xs)
  | '0' <= x && x <= '9' = (IntToken (read [x]), xs)
  | x == '+'             = (OpToken x, xs)
  | x == '-'             = (OpToken x, xs)
  | x == '*'             = (OpToken x, xs)
  | x == '/'             = (OpToken x, xs)
  | x == '('             = (OpenPToken, xs)
  | x == ')'             = (ClosePToken, xs)
  | otherwise            = (NullToken, xs)
parse "" = (NullToken, "")

main :: IO ()
main = putStrLn (show (parse "31+4"))