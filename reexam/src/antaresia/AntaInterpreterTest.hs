module Main where
import Test.HUnit
import Data.Either as Either
import AntaInterpreter
import AntaAST



--different tests
test1 = [("a",(IntConst 1))]
test2 = [("a",(TrueConst))]
test3 = [("a",(TrueConst)),("b",(Name "a"))]
test4 = [("a",(TrueConst)),("b",(Name "c"))]
test5 = [("a",Range (A3 (IntConst 1) (IntConst 12) (IntConst 3)))]
test6 = [("a",Plus (IntConst 1) (IntConst 3))]
test7 = [("a",Minus (IntConst 1) (IntConst 3))]
test8 = [("a",Mult (IntConst 3) (IntConst 5))]
test9 = [("a",Div (IntConst 6) (IntConst 5))]
test10 = [("a",Div (IntConst 6) (IntConst 0))]
test11 =[("a",Modulus (IntConst 15) (IntConst 2))]
test12 =[("a",Equal (IntConst 15) (IntConst 12))]
test13 =[("a",Equal (IntConst 15) (TrueConst))]
test14 =[("a",ListComp (Simple [IntConst 1,IntConst 2,IntConst 3]))]



--function that checks if the parsing results into Error or Program
checkResult :: Either String Result -> Bool
checkResult (Right _) = True
checkResult _ = False


--test asserts
testParse1 = TestCase $ assertBool "Test IntConst" $ checkResult (AntaInterpreter.runProg  test1)
testParse2 = TestCase $ assertBool "Test bool expression" $ checkResult (AntaInterpreter.runProg test2)																				
testParse3 = TestCase $ assertBool "Test expression Name Name" $ checkResult (AntaInterpreter.runProg  test3)	
testParse4 = TestCase $ assertBool "Test expression Name Name where argument is invalid" $ not $ checkResult (AntaInterpreter.runProg  test4)
testParse5 = TestCase $ assertBool "Test range with 3 args" $ checkResult (AntaInterpreter.runProg  test5)	
testParse6 = TestCase $ assertBool "Test plus expression" $ checkResult (AntaInterpreter.runProg  test6)
testParse7 = TestCase $ assertBool "Test minus epression" $ checkResult (AntaInterpreter.runProg  test7)
testParse8 = TestCase $ assertBool "Test multiply expression" $ checkResult (AntaInterpreter.runProg  test8)
testParse9 = TestCase $ assertBool "Test div expression" $ checkResult (AntaInterpreter.runProg  test9)
testParse10 = TestCase $ assertBool "Test div by 0" $ not $ checkResult (AntaInterpreter.runProg  test10)
testParse11 = TestCase $ assertBool "Test modulus" $ checkResult (AntaInterpreter.runProg test11)
testParse12 = TestCase $ assertBool "Left recursion" $ checkResult (AntaInterpreter.runProg test12)
testParse13 = TestCase $ assertBool "Test equals between variables of the same type" $ checkResult (AntaInterpreter.runProg test13)
testParse14 = TestCase $ assertBool "Test equals between variables of different types" $ not $ checkResult (AntaInterpreter.runProg test14)

--list of tests
tests = TestList [TestLabel "AntaParser testSuite" $ TestList [testParse1,testParse2, testParse3, testParse4, testParse5, testParse6, testParse7, testParse8, testParse9, testParse10, testParse11, testParse12, testParse13, testParse14]]

--main
main = do
	runTestTT tests









