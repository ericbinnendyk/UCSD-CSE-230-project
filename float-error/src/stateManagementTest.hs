-- some example code (probably with bugs) to demonstrate how state processing can work
-- the example state has the current value and the history of expressions
-- we can also add more complicated things to the state, like the symbol table

-- for the state of the REPL, it consists of a State object and a FloatWithError object representing the result of the previous calculation
data StateVal a =
  | SV (State a, a)
  | Error String
-- the type a is going to be FloatWithError in practice

data State = [(Expr, a)] -- let's make the State type store the history of calculations for good measure

instance Monad of StateVal where
  | (>>=) = stateValBind
  | return = stateValReturn

stateValBind :: StateVal a -> (a -> StateVal b) -> StateVal b
stateValBind sv f = case sv of
  | SV (s, v) => case fv of
    | SV (s1, v1) => StateVal (s ++ s1, v1) -- concatenating two histories from instruction sets to make longer instructions
    | Error str1 => Error str1
  | Error str => Error str

stateValReturn :: a -> StateVal a
stateValReturn v -> StateVal ([], v)

