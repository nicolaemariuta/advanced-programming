{- 
  Example code produced for Advanced Programming lecture.  

  A basic interpreter for the λ-calculus, with a special Wrong value
  in the semantic domain.

  Date: Sep 11, 2012
  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
type Name = String
data Term = Var Name
          | Lam Name Term
          | App Term Term
          | Con Int
          | Add Term Term
     deriving (Eq, Show)

data Value = Wrong
           | Num Int
           | Fun (Value -> Value)

termid = (Lam "x" (Var"x"))
mysteryTerm = App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 11) (Con 10))

instance Show Value where
    show Wrong   = "Wrong"
    show (Num n) = "Num "++show n
    show (Fun f) = "<Fun>"

type Env = [(Name, Value)]

lookupEnv :: Name -> Env -> Value
lookupEnv x env = 
  case bindings of
    []     -> failure
    v : _  -> v
    where bindings = [v| (k,v) <- env, k == x]

interp :: Term -> Env -> Value
interp (Var x) env     = lookupEnv x env
interp (App t1 t2) env = let f = interp t1 env
                             v = interp t2 env
                         in  apply f v
interp (Lam x b) env   = Fun (\v -> interp b ((x,v):env)) 
interp (Con n) env     = Num n
interp (Add t1 t2) env = let x = interp t1 env
                             y = interp t2 env
                         in  add x y

apply :: Value -> Value -> Value
apply (Fun f) v = f v
apply _ _       = failure

add :: Value -> Value -> Value
add (Num n) (Num m) = Num(n+m)
add _ _ = failure

failure :: Value
failure = Wrong
