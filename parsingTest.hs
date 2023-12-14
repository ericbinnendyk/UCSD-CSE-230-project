-- datatype representing a single token
-- includes things like IntToken 5 and OpToken '+'
data Token =
  IntToken Int
  OpToken Char
  NullToken

-- so far, this only parses single-digit numbers as tokens
parse :: String -> (Token, String)
parse x:xs
  | '0' <= x && x <= '9' = (IntToken toInt x, xs)
  | x == '+'             = (OpToken x, xs)
  | x == '-'             = (OpToken x, xs)
  | x == '*'             = (OpToken x, xs)
  | x == '/'             = (OpToken x, xs)
  | otherwise            = (NullToken, xs)
parse "" = (NullToken, "")

main = putStr "replace me"
