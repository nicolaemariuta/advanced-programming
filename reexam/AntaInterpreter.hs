{-# OPTIONS_GHC -Wall #-}
module AntaInterpreter
       ( runProg
       , Error (..)
       )
       where

import AntaAST

-- You might need the following imports
-- import Control.Applicative
-- import Control.Monad
-- import qualified Data.Map as Map
-- import Data.Map(Map)



-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Error = Error String
             deriving (Show, Eq)

-- | A mapping from names to values
type Env = ...

-- | The basic monad in which execution of a Antaresia program takes
-- place.  Maintains the global state and whether or not an error has
-- occurred.
data AntaM a = AntaM { runAntaM :: Env -> ... }

instance Functor AntaM where
  fmap = liftM

instance Applicative AntaM where
  pure = return
  (<*>) = ap

instance Monad AntaM where
  return = undefined
  (>>=) = undefined
  fail = undefined

{-
modify :: (Env -> Env) -> AntaM ()
modify f = undefined

updateEnv :: Name -> Value -> AntaM ()
updateEnv name val = undefined
-}


-- | The monad in which to evaluate expressions, expression cannot
-- modify the environment but can still query/read the environment,
-- and the evaluation of expressions can fail.
data PureM a = PureM { runPureM :: Env -> ... }

instance Functor PureM where
  fmap = liftM

instance Applicative PureM where
  pure = return
  (<*>) = ap

instance Monad PureM where
  return = undefined
  (>>=) = undefined
  fail = undefined

{-
-- ^ lookup the value of a name in the environment
getValue :: Name -> PureM Value
getValue name = undefined

local :: (Env -> Env) -> PureM a -> PureM a
local f m = undefined

liftP :: PureM a -> AntaM a
liftP pur = undefined
-}



-- ^ function for implementing the behavoir of the Python function range
range :: Integer -> Integer -> Integer -> [Integer]
range start stop step = 
        do
            b <- start + step
            return [start,b..stop]


evalExpr :: Expr -> PureM Value
evalExpr expr = undefined

{-
listIter :: PureM a -> ListIter -> PureM [a]
listIter body iter = undefined
-}


decl :: Decl -> AntaM (Name, Value)
decl (n, expr) = undefined
  
program :: Program -> AntaM Result
program prog = undefined

runProg :: Program -> Either Error Result
runProg prog = undefined
