-- Radu Ionita, Nicolae Maruta
module Curve where

import System.Environment()
import Text.Printf
import System.IO()

data Point = Point {x::Double, y::Double} deriving (Show)
type Curve = [Point]

point :: (Double, Double) -> Point
point p1 = Point {x = (fst p1), y = (snd p1)}

comparePoints :: Point -> Point -> Bool
comparePoints p1 p2 = (truncate ((x p1) * 100)) - (truncate ((x p2) * 100)) == 0 
				   && (truncate ((y p1) * 100)) - (truncate ((y p2) * 100)) == 0

instance Eq Point where
	p1 == p2 = comparePoints p1 p2
	p1 /= p2 = not (comparePoints p1 p2)

curve :: Point -> [Point] -> Curve
curve noPoints [] = [noPoints]
curve onePoint [value] = [onePoint]
curve curvePoint list = curvePoint:list

connect :: Curve -> Curve -> Curve
connect c1 c2 = c1 ++ c2 

rotatePoint :: Double -> Point -> Point
rotatePoint anglePoint thePoint = Point {x = (x thePoint) * c + (y thePoint) * s, y = (y thePoint) * c - (x thePoint) * s}
		where (s,c) = (sin (anglePoint / 180 * pi), cos (anglePoint / 180 * pi))

rotate :: Curve -> Double -> Curve
rotate curvePoints theta
		= map (rotatePoint theta) curvePoints

translatePoint :: Point -> Point -> Point
translatePoint cPoint pPoint = Point {x=(x cPoint) + (x pPoint), y=(y cPoint) + (y pPoint)}
	
translate :: Curve -> Point -> Curve
translate curvePoints otherPoint = map (translatePoint otherPoint) curvePoints

data Axis = Vertical | Horizontal deriving (Eq,Show)

reflectPoint :: Axis -> Double -> Point -> Point
reflectPoint p aValue cPoint =
	if p == Horizontal
		then Point { x = (x cPoint), y = aValue - (y cPoint) + aValue} 
		else Point { x = aValue - (x cPoint) + aValue, y = (y cPoint)}

reflect :: Curve -> Axis -> Double -> Curve
reflect points axis axisValue = map (reflectPoint axis axisValue) points

minMaxPointValues :: (Point,Point) -> Point -> (Point,Point)
minMaxPointValues (point1, point2) comparePoint = (Point {x=(min (x point1) (x comparePoint)), y=(min (y point1) (y comparePoint))},
									  	 	      Point {x=(max (x point2) (x comparePoint)), y=(max (y point2) (y comparePoint))})

bbox :: Curve -> (Point, Point)
bbox [] = error "there is no item in the box"
bbox (_:[]) = error "too few param"
bbox points = let (minP, maxP) = (
					Point {x = (min (x (head points)) (x (head (tail points)))), y = (min (y (head points)) (y (head (tail points))))},
					Point {x = (max (x (head points)) (x (head (tail points)))), y = (min (y (head points)) (y (head (tail points))))})
			  in foldl minMaxPointValues (minP, maxP) (tail (tail points))
			
height :: Curve -> Double
height curvePoints = abs((y (fst a))) + (y (snd a))
	where a = bbox curvePoints

width :: Curve -> Double
width curvePoints = abs((x (fst a))) + (x (snd a))
	where a = bbox curvePoints

toList :: Curve -> [Point]
toList [] = []
toList (fElem:listPoints) = fElem : (toList listPoints)

printPoints :: [Point] -> String
printPoints [] = error "no elem in the list"
printPoints (_:[]) = "</g></svg>"
printPoints (firstPoint:points) = "<line style=\"stroke-width: 2px; stroke: black; fill:white\" x1=\"" 
            ++ (printf "%.2f" (x firstPoint)) ++ 
            "\" x2=\"" ++ (printf "%.2f" (x (head points))) ++ 
            "\" y1=\"" ++ (printf "%.2f" (y firstPoint)) ++ 
            "\" y2=\"" ++ (printf "%.2f" (y (head points))) ++ "\" />" ++ (printPoints points)

toSVG :: Curve -> String 
toSVG points = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
			 ++ (show (ceiling (width points))) ++ "px\" height=\""
			 ++ (show (ceiling (height points))) ++ "px\" version=\"1.1\"><g>" 
			 ++ printPoints (toList points)

toFile :: Curve -> FilePath -> IO ()
toFile curvePoints filePath =
	    writeFile filePath (toSVG curvePoints)

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
