{-
  Example code produced "during" Advanced Programming lecture 1.

  Whirlwind tour of Haskell

  Date: Sep 2, 2014
  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

simpsons :: [(String, Int)]  -- type signature
simpsons = [ ("Homer", 42)
           , ("Marge", 38)
           , ("Bart", 9)
           , ("Lisa", 8)
           , ("Maggie", 1)]

baz = (True, 23.5, "Homer, again")

s1 :: String
s1 = "Bart"

add :: Int -> Int -> Int
add x y = x+y

fancyAdd = add 38


digits = [0..9]
evenDigits = [x | x <- digits, x `mod` 2 == 0]


nats = [0 ..]
evenNats = [x | x <- nats, x `mod` 2 == 0]


startFrom s = s : startFrom (s+1)

len [] = 0
len (_ : t) = 1 + len t

q [] = []
q (x:xs) = q sxs ++ [x] ++ q lxs
    where sxs = [a | a <- xs, a <= x]
          lxs = [b | b <- xs, b > x]

-- Type tour

type Pos = (Int, Int)
data Direction = North | South | East | West
            deriving (Eq, Show)

data Student = Student {name :: String, knowsHaskell :: Bool}
               deriving (Eq, Show)

youngTroels = Student {name="Troels", knowsHaskell = False}

followAP :: Student -> Student
followAP stud = stud{knowsHaskell = True}

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)


-- Polymorphic types

type Assoc a = [(String, a)]

--findAssoc :: String -> Assoc a -> Either ??? a
findAssoc :: String -> Assoc a -> Maybe a
findAssoc key assoc =
  case bindings of
    []    -> Nothing
    x : _ -> Just x
 where bindings = [ val | (k, val) <- assoc, k == key]


{- The following two types are part of the prelude -}
--  data Maybe a = Nothing | Just a
--  data Either a b = Left a | Right b

-- Recursive types and syntax trees

data Nat = Zero | Succ Nat --
         deriving (Read, Show, Ord)

-- We make Nat an instance of Eq by hand (but it would have been easier to derrive it)

instance Eq Nat where
  Zero == Zero	   = True
  Succ n == Succ m = n == m
  _ == _	   = False



addN :: Nat -> Nat -> Nat
addN x Zero = x
addN x (Succ n) = Succ(addN x n)





data Expr = Con Int
          | Add Expr Expr
     deriving (Eq, Show, Read, Ord)

value :: Expr -> Int
value (Con n) = n
value (Add x y) = value x + value y

convList = map Con
add1 = Add $ Con 1
