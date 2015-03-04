{-
Example code produced for Advanced Programming lecture.

Main purpose is to show how to generate recursive types.
-}
import Test.QuickCheck
import Control.Monad (liftM, liftM2, ap)
import Control.Applicative

data Expr = Con Int
          | Add Expr Expr
     deriving (Eq, Show, Read, Ord)

value :: Expr -> Int
value (Con n) = n
value (Add x y) = value x + value y

prop_com_add x y = value (Add x y) == value (Add y x)


instance Arbitrary Expr where
  arbitrary = expr

expr =  oneof [liftM Con arbitrary,
               Add <$> expr <*> expr]

-- -- Take 2: using sized generators
-- instance Arbitrary Expr where
--   arbitrary = expr2

-- expr2 = sized exprN
-- exprN 0 = liftM Con arbitrary
-- exprN n = oneof [liftM Con arbitrary,
--                  liftM2 Add subexpr subexpr]
--   where subexpr = exprN (n `div` 2)
