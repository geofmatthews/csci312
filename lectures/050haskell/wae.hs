

type Identifier = String
type Value = Int
type Env = [(Identifier, Value)]
data WAE = Num Int
         | Add WAE WAE
         | Id Identifier
         | With Identifier WAE WAE

interp :: WAE -> Env -> Value
interp (Num n) env = n
interp (Add lhs rhs) env = interp lhs env + interp rhs env
interp (Id i) env = Main.lookup i env
interp (With bound_id named_expr bound_body) env =
  interp bound_body
      (extend env bound_id (interp named_expr env))

lookup :: Identifier -> Env -> Value
lookup var ((i,v):r)
  | (var == i) = v
  | otherwise = Main.lookup var r

extend :: Env -> Identifier -> Value -> Env
extend env i v = (i,v):env

