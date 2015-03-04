type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x,y) = (x, y-1)
move East  (x,y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos 
moves (d:ds) p = move d (moves ds p)
moves [] p = p

myMoves = [North, North, North]

-------

-- Nat = it is a function Zero that returns an int | a function Succ that takes a value or Nat and returns an int
data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord) 

addN :: Nat -> Nat -> Nat
addN x Zero = x
addN x (Succ n) = Succ(addN x n)

multiply :: Nat -> Nat-> Nat
multiply x Zero = Zero
multiply Zero x = Zero
multiply x (Succ n) = addN x (multiply x n) 

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + (nat2int n)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x-1))

---------------------

-- Every element in the left subtree is <= node value.
data Tree = Leaf | Node Int Tree Tree
    deriving (Eq, Show, Read, Ord)

----------------------
	
--Expr = it is a function named Con that takes an Int	
data Expr = Con Int
          | Add Expr Expr
		  | Mul Expr Expr
		  | Div Expr Expr
		  | Sub Expr Expr
     deriving (Eq, Show, Read, Ord) 

value :: Expr -> Int
value (Con n) = n
value (Add x y) = value x + value y 	
value (Mul x y) = value x * value y
value (Div x y) = value x `div` value y
value (Sub x y) = value x - value y
	
add :: Num n => n -> n -> n
add = \ x y -> x+y


