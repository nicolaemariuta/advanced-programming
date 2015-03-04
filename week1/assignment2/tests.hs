module Main where
import Test.HUnit
import Curves

curve1 = curve (point (0,0)) []
curve2 = curve (point (1,0)) [Point{x=1,y=1}]

test1 = TestCase $ asserBool "Curve1" $ 