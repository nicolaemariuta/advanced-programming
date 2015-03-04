

data Point = Double  Double deriving (Eq, Show)

type Curve = [Point]	
data Axis = Vertical | Horizontal

comparePoints :: Point -> Point -> Bool
comparePoints a b
	= (truncate ((x a) * 100)) - (truncate ((x b)* 100)) == 0 
	&& (truncate ((y a) * 100)) - (truncate ((y b) * 100)) == 0


