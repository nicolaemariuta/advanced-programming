import Curves

run :: IO()
run = toFile (hilbert $ hilbert $ hilbert $ hilbert $ curve (point (0,0)) []) "D:/master/advanced programming/week1/assignment2/hilbert4.svg"
--run = toFile (hilbert $ hilbert $ hilbert $ hilbert $ curve (point (5,8)) [Point{x=3,y=4}]) "D:/master/advanced programming/week1/assignment2/hilbert4.svg"