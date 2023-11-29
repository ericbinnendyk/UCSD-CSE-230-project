data FloatWithError
  = FWE Float Float
  deriving (Show)

add_fwe :: FloatWithError -> FloatWithError -> FloatWithError
add_fwe (FWE l1 h1) (FWE l2 h2) = FWE (l1 + l2) (h1 + h2)

mul_fwe :: FloatWithError -> FloatWithError -> FloatWithError
mul_fwe (FWE l1 h1) (FWE l2 h2)
  | l1 >= 0 && l2 >= 0                       = FWE (l1 * l2) (h1 * h2)
  | l1 <= 0 && h1 >= 0 && l2 >= 0            = FWE (l1 * h2) (h1 * h2) -- wrong?
  | l1 <= 0 && h1 >= 0 && l2 <= 0 && h2 >= 0 = FWE (min (l1 * h2) (h1 * l2)) (max (l1 * l2) (h1 * h2))
  | l1 >= 0 && l2 <= 0 && h2 >= 0            = FWE (h1 * l2) (h1 * h2) -- wrong?
  | l1 <= 0 && h1 >= 0 && h2 <= 0            = FWE (h1 * l2) (l1 * h2)
  | h1 <= 0 && l2 <= 0 && h2 >= 0            = FWE (l1 * h2) (h1 * l2)
  | l1 >= 0 && h2 <= 0                       = FWE (h1 * l2) (l1 * h2)
  | h1 <= 0 && l2 >= 0                       = FWE (l1 * h2) (h1 * l2)
  | h1 <= 0 && h2 <= 0                       = FWE (l1 * l2) (h1 * h2)

data Expression
  = ExprAdd Expression Expression
  | ExprSub Expression Expression
  | ExprMul Expression Expression
  | ExprDiv Expression Expression
  | ExprNeg Expression
  | ExprNum FloatWithError

-- I'm going to write this non-monadically for now. Later I will switch to writing it with monads to deal with errors like division by zero.
evalExpr :: Expression -> FloatWithError
evalExpr expr =
  case expr of
    ExprAdd e1 e2 -> add_fwe (evalExpr e1) (evalExpr e2)
    ExprMul e1 e2 -> mul_fwe (evalExpr e1) (evalExpr e2)
    ExprSub e1 e2 -> error "Subtraction not implemented yet"
    ExprDiv e1 e2 -> error "Division not implemented yet"
    ExprNeg e1    -> error "Negation not implemented yet"
    ExprNum x     -> x

-- main = putStrLn (show (mul_fwe (add_fwe (FWE 0.5 0.9) (FWE 0.3 0.6)) (FWE (-0.2) 0.2)))
main = putStrLn (show (evalExpr (ExprMul (ExprAdd (ExprNum (FWE 0.5 0.9)) (ExprNum (FWE 0.3 0.6))) (ExprNum (FWE (-0.2) 0.2)))))