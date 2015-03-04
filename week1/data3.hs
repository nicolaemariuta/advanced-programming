data Car a b c = Car { company :: a
					, model :: b
					, year :: c
					} deriving (Show)
					
tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t ->Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m) 

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Person = Person {firstName :: String
					, lastName :: String
					, age :: Int
					} deriving (Eq, Show, Read)
					
data Boo = False | True deriving (Ord)
					
