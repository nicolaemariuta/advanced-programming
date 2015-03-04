{-
Example code produced "during" Advanced Programming lecture.

Evaluate simple expression to values
-}
module Expr where

data Expr = Con Int
          | Add Expr Expr
          | Div Expr Expr
     deriving (Eq, Show)


value :: Expr -> Maybe Float
value (Con n) = Just $ fromIntegral n
value (Add x y) =
  case value x of
    Just n -> case value y of
                Just m -> Just $ n+m
                Nothing -> Nothing
    Nothing -> Nothing
value (Div x y) =
  case value y of
    Just n -> if n == 0.0 then Nothing
              else case value x of
                     Just m -> Just$ m / n
                     Nothing -> Nothing
    Nothing -> Nothing


value2 :: Expr -> Maybe Float
value2 (Con n) = return $ fromIntegral n
value2 (Add x y) = do
  f1 <- value2 x
  f2 <- value2 y
  return $ f1 + f2
value2 (Div x y) = do
  f1 <- value2 x
  f2 <- value2 y
  if f2 == 0.0 then Nothing
  else return $ f1/f2


-- just some show-off using type classes
instance Num Expr where
  a + b = a `Add` b
  fromInteger n = Con$ fromInteger n

  a - b = undefined
  a * b = undefined
  abs = undefined
  signum = undefined

instance Fractional Expr where
  a / b = a `Div` b
  fromRational r = undefined
