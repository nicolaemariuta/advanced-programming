{-
Example code produced "during" Advanced Programming lecture 2.

Simple module for natural numbers, illustrating how to make an
instance of the Num type class.

Date: Sep 9, 2014
Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module Natural where

data Nat = Zero | Succ Nat
         deriving (Show, Read)

add x Zero = x
add x (Succ n) = add (Succ x) n

minus :: Nat -> Nat -> Nat
minus _ _ = undefined


instance Eq Nat where
  Zero == Zero	   = True
  Succ n == Succ m = n == m
  _ == _	   = False

instance Num Nat where
  a + b = a `add` b
  a - b = a `minus` b
  a * b = error "Undefined" -- Just to show something different than
                            -- undefined, it is perfectly possible to
                            -- define multiplication for natural
                            -- numbers. You should do it.

  fromInteger 0 = Zero
  fromInteger n = Succ $ fromInteger (n-1)

  abs = undefined
  signum = undefined
