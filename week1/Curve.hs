

type Point = (Double, Double)

type Curve = [Point]	
data Axis = Vertical | Horizontal


point :: (Double, Double) -> Point
point (a, b) = (a, b)



curve :: Point -> [Point] ->Curve
curve p ss = p : ss




connect :: Curve -> Curve -> Curve
connect c1 c2 = c1 ++ c2

rotate :: Curve -> Double -> Curve
rotate [(x,y)] a = [(x*cos a - y* sin a,y*cos a + x* sin a)]

translate :: Curve -> Point -> Curve
translate [(x,y)] (a,b) = [(x+a,y+b)]


reflect :: Curve -> Axis -> Double -> Curve
reflect [(x,y)] Horizontal a = if a < y
							   then [(x,y-2*(y-a))]
							   else [(x,y+2*(a-y))]
							   
reflect [(x,y)] Vertical a = if x < a
							   then [(x-2*(x-a),y)]
							   else [(x+2*(a-x),y)]
							   
bbox :: Curve -> (Point, Point)
bbox c = ((maximumx c, maximumy c),(minimumx c, minimumy c))


maximumx :: Curve -> Double
maximumx [(x,y)] = x  
maximumx((x,y):xs) = max x (maximumx xs)

maximumy :: Curve -> Double
maximumy [(x,y)] = y  
maximumy((x,y):xs) = max y (maximumy xs)

minimumx :: Curve -> Double
minimumx [(x,y)] = x  
minimumx((x,y):xs) = max x (minimumx xs)

minimumy :: Curve -> Double
minimumy [(x,y)] = y  
minimumy((x,y):xs) = max y (minimumy xs)

width :: Curve -> Double
width c = abs x - y
	where x = fst fst bbox c
		  y = fst snd bbox c
		  

height :: Curve -> Double
height c = abs x - y
	where x = snd fst bbox c
		  y = snd snd bbox c



toList :: Curve -> [Point]
toList curve = curve



hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c Horizontal 0

          c0 = ch `rotate` (-90) `translate` (point (w+p+w, h+p+h))
          c1 = c `translate` (point (w+p+w, h))
          c2 = c
          c3 = ch `rotate` 90 `translate` (point (0, h+p))				 








