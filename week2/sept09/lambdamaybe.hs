{- 
  Example code produced for Advanced Programming lecture.  

  A basic interpreter for the λ-calculus in monadic style, using the
  Maybe monad to modelling computations that can "go wrong" rather than
  having a special Wrong value in the semantic domain.

  Date: Sep 9, 2014
  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

type M a = Maybe a

type Name = String
data Term = Var Name
          | Lam Name Term
          | App Term Term
          | Con Int
          | Add Term Term
     deriving (Eq, Show)

data Value = Num Int
           | Fun (Value -> M Value)

instance Show Value where
    show (Num n) = show n
    show (Fun f) = "<Fun>"

termid = (Lam "x" (Var"x"))
term42 = App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 11) (Con 10))

termWrong = App (Con 42) termid



type Env = [(Name, Value)]

lookupEnv :: Name -> Env -> M Value
lookupEnv x env = case bindings of
                 []     -> failure
                 (v:vs) -> return v
    where bindings = [v| (k,v) <- env, k == x]


interp :: Term -> Env -> M Value
interp (Var x) env     = lookupEnv x env
interp (App t1 t2) env = do f <- interp t1 env
                            v <- interp t2 env
                            apply f v
interp (Lam x b) env   = return $ Fun (\v -> interp b ((x,v):env))
interp (Con n) env     = return $ Num n
interp (Add t1 t2) env = do x <- interp t1 env
                            y <- interp t2 env
                            add x y

apply :: Value -> Value -> M Value
apply (Fun f) v = f v
apply _ _       = failure

add :: Value -> Value -> M Value
add (Num n) (Num m) = return (Num(n+m))
add _ _ = failure

failure :: M Value
failure = Nothing
