data Student = Student { name :: String,
						knowsHaskell :: Bool}
						
followAP :: Student -> Student
followAP stud = stud{knowsHaskell = True}