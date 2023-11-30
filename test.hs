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

sub_fwe (FWE l1 h1) (FWE l2 h2) = add_fwe (FWE l1 h1) (neg_fwe (FWE l2 h2))

neg_fwe (FWE l1 h1) = (FWE (-h1) (-l1))

-- Multiplicative inverse, an auxiliary function used to define division
-- I could give a special case for when the lower or upper bound of the range is exactly zero, and have the lower or upper bound of the range of the inverse be positive or negative infinity.
-- But that would require checking whether one of the bounds of the range was exactly 0. We aren't supposed to do exact equality checks for floats.
-- So I would need a different constructor for the FloatWithError type, that specifies that one of the bounds is *exactly* zero.
inv_fwe (FWE l1 h1)
  | (l1 > 0 && h1 > 0) || (l1 < 0 && h1 < 0) = (FWE (1/h1) (1/l1))
  | otherwise                                = (FWE (-Infinity) (Infinity)) -- technically this isn't the exact set of values it could take, but it's the minimal interval containing that set

div_fwe (FWE l1 h1) (FWE l2 h2) = mul_fwe (FWE l1 h1) (inv_fwe (FWE l2 h2))

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
    ExprSub e1 e2 -> sub_fwe (evalExpr e1) (evalExpr e2)
    ExprDiv e1 e2 -> error "Division not implemented yet"
    ExprNeg e1    -> neg_fwe (evalExpr e1)
    ExprNum x     -> x

-- main = putStrLn (show (mul_fwe (add_fwe (FWE 0.5 0.9) (FWE 0.3 0.6)) (FWE (-0.2) 0.2)))
main = putStrLn (show (evalExpr (ExprMul (ExprAdd (ExprNum (FWE 0.5 0.9)) (ExprNum (FWE 0.3 0.6))) (ExprNum (FWE (-0.2) 0.2)))))