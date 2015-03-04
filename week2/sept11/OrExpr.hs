{-
Example code produced for Advanced Programming lecture.

Evaluate simple expression to values, dealing with multible values.

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module OrExpr where

import Control.Monad(liftM2, join)

data Expr = Con Int
          | Add Expr Expr
          | Or  Expr Expr
     deriving (Eq, Show, Read, Ord)

newtype Multi a = M { unM :: [a] }
                deriving (Eq, Show)

instance Monad Multi where
  return x = M [x]
  (M xs) >>= f  = M $ concatMap (unM . f) xs


noValue = M []
x `orElse` y = M [x,y]



value :: Expr -> Multi Int
value (Con n)   = return n
value (Add x y) = liftM2 (+) (value x) (value y)
value (Or x y)  = join $ liftM2 orElse (value x) (value y)


-- just some show-off using type classes
instance Num Expr where
  a + b = a `Add` b
  fromInteger n = Con$ fromInteger n

  _ - _ = undefined
  _ * _ = undefined
  abs = undefined
  signum = undefined


