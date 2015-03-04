module Stuff where

import Data.List(permutations, nub)

data CW = CW[Int] [((Int, Int), (Int, Int))][String]

type CWSolution = [String]

example :: CW
example = CW[5,4,6]
			[ ((0,1), (1, 1))
			,((0,3), (2, 0))
			]
			["word","tower", "black", "coffin", "enigma"]

ourPermutations :: [String] -> Int -> [[String]]
ourPermutations ss n = nub $ map (take n) $ permutations ss

--brute force!
solve :: CW -> [CWSolution]
solve (CW wordLengths crossTuples startWords)
	= filter (isSolution testSolutions crossTuples) perms
	where wordsN = length wordLengths
		  perms = ourPermutations startWords wordsN
		  
		  
isSolution :: [Int] -> 
			  [((Int, Int), (Int, Int))]-> 
			  CWSolution ->
			  Bool
isSolution wordLengths crossTuples strings
	= all isOk crossTuples
	where isOk ((idx0, place0), (idx1, place1))
			= string !! idx0 !! place0
				== strings !! idx1 !! place1
		lengthOk :: (Int, String) -> Bool

--------------------------------------------------------------------------------------------
data Name = Name String

--Name "Niels" == Name "Nils"

instance Eq Name where
	(Name "Niels") == (Name "Nils") = True 
	_==_=False
	

data Op = Plus

data Prog = BinEpr Op Int Int
prettyPrintProg :: Prog -> String


test0 = reflect someCurve Horizontal 388 == true

---------------------------------------------------------------------------------------------------------------------


comparePoints :: Point -> Point -> Bool
comparePoints a b
	= (truncate ((x a) * 100)) - (truncate ((x b)* 100)) == 0 
	&& (truncate ((y a) * 100)) - (truncate ((y b) * 100)) == 0