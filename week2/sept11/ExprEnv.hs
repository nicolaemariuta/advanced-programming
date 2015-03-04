module ExprReader where

data Expr = Con Int
          | Add Expr Expr
          | Var String
          | Let String Expr Expr
     deriving (Eq, Show)

type Env = String -> Int

newtype Eval a = E { unE :: Env -> a }

instance Monad Eval where
  return x = E $ \ _ -> x

  -- (>>=) :: Eval a -> (a -> Eval b) -> Eval b
  (E m) >>= f = E $ \ env -> let v1 = m env in unE (f v1) env

lookupEnv :: String -> Eval Int
lookupEnv v = E $ \ env -> env v

update :: String -> Int -> Eval Env
update x v = E $ \ env -> \y -> if x == y then v else env y 

emptyEnv :: Env
emptyEnv = \ _ -> 0 

localEnv :: Env -> Eval a -> Eval a
localEnv env ev = return (unE ev env)

value :: Expr -> Eval Int
value (Con n)   = return n
value (Add x y) = do 
  f1 <- value x
  f2 <- value y
  return$ f1 + f2
value (Var x) =
  lookupEnv x
value (Let x e body) = do
  v <- value e
  newEnv <- update x v
  localEnv newEnv $ value body




-- instance Functor Eval where
--   -- fmap :: (a -> b) -> Eval a -> Eval b
--   fmap f m = m >>= return . f






-- instance Applicative Eval where
--   pure = return
--   df <*> dx = ...
