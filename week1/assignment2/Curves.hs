-- Radu Ionita, Nicolae Maruta
module Curves where

import System.Environment()
import Text.Printf
import System.IO()

data Point = Point {x::Double, y::Double} deriving (Show)
data Curve = Curve Point [Point] deriving (Show)

point :: (Double, Double) -> Point
point p1 = Point {x = (fst p1), y = (snd p1)}

comparePoints :: Point -> Point -> Bool
comparePoints p1 p2 = ( (x p1) - (x p2)) < 0.01 
				   && ( (y p1) - (y p2)) < 0.01 

instance Eq Point where
	p1 == p2 = comparePoints p1 p2

curve :: Point -> [Point] -> Curve
curve p ps = Curve p ps 

connect :: Curve -> Curve -> Curve
connect (Curve p ps) (Curve o os) = Curve p (ps++(o:os))

rotatePoint :: Double -> Point -> Point
rotatePoint anglePoint thePoint = Point {x = (x thePoint) * c + (y thePoint) * s, y = (y thePoint) * c - (x thePoint) * s}
		where (s,c) = (sin (anglePoint / 180 * pi), cos (anglePoint / 180 * pi))

cmap :: (Point -> Point) -> Curve -> Curve
cmap f (Curve p ps) = (Curve (f p) (map f ps))
		
rotate :: Curve -> Double -> Curve
rotate curvePoints theta
		= cmap (rotatePoint theta) curvePoints

translatePoint ::  Double -> Double -> Point ->Point
translatePoint  deltax deltay cPoint = Point {x=(x cPoint) +  deltax, y=(y cPoint) + deltay}
	
translate :: Curve -> Point -> Curve
translate curvePoints@(Curve p ps) otherPoint = cmap (translatePoint deltax deltay) curvePoints
														where
															deltax = (x otherPoint) - (x p)
															deltay = (y otherPoint) - (y p)

data Axis = Vertical | Horizontal deriving (Eq,Show)

reflectPoint :: Axis -> Double -> Point -> Point
reflectPoint Vertical aValue cPoint = Point { x = 2 * aValue - (x cPoint), y = y cPoint} 
reflectPoint Horizontal aValue cPoint = Point { x = (x cPoint), y = 2 * aValue -(y cPoint)}


reflect :: Curve -> Axis -> Double -> Curve
reflect points axis axisValue = cmap (reflectPoint axis axisValue) points



minMaxPointValues :: (Point,Point) -> Point -> (Point,Point)
minMaxPointValues (point1, point2) comparePoint = (Point {x=(min (x point1) (x comparePoint)), y=(min (y point1) (y comparePoint))},
									  	 	      Point {x=(max (x point2) (x comparePoint)), y=(max (y point2) (y comparePoint))})

bbox :: Curve -> (Point, Point)
bbox (Curve p ps) = foldl minMaxPointValues (p, p) ps
		
height :: Curve -> Double
height curvePoints = abs((y (fst a))) + (y (snd a))
	where a = bbox curvePoints

width :: Curve -> Double
width curvePoints = abs((x (fst a))) + (x (snd a))
	where a = bbox curvePoints

toList :: Curve -> [Point]
toList (Curve p ps) = p:ps

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

		  
--main :: IO()
--main = toFile (hilbert $ hilbert $ hilbert $ hilbert $ curve (point (0,0)) []) "D:/master/advanced programming/week1/assignment2/test.svg"