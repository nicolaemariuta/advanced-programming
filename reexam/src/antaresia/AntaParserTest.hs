module Main where
import Test.HUnit
import Data.Either as Either
import AntaParser
import AntaAST


--files with code for resting
test1 = "S = [x*x for x in range(10)] M = [x for x in S if x % 2 == 0] noprimes = [j for i in range(2, 8) for j in range(i*2, 50, i)] primes = [x for x in range(2, 50) if x not in noprimes]"
test2 = "a=1+2"
test3 = "a+*2"
test4 = "a = 1 n = [True,False,2+3]"
test5 = "True,False,2+3"
test6 = "M = [x for x in S if [j for i in range(2, 8) for j in range(i*2, 50, i)]]"
test7 = "a=1+2//5 in True"
test8 = "\n a= \t 1 \t +  \n        2  "
test9 = "a=[1in2not in3in4]"
test10 = "a=[1*2//3%5]"
--trees that are expected for each file1
resultFile1 = [("a",Plus (IntConst 1) (IntConst 2))]


--function that checks if the parsing results into Error or Program
checkResult :: Either Error Program -> Bool
checkResult (Right _) = True
checkResult _ = False

--test asserts
testParse1 = TestCase $ assertBool "Parsing code in appendix" $ checkResult (AntaParser.parseString  test1)
testParse2 = TestCase $ assertBool "Parsing simple add" $ checkResult (AntaParser.parseString  test2)																				
testParse3 = TestCase $ assertBool "Parsing wrong expression" $ not $ checkResult (AntaParser.parseString  test3)	
testParse4 = TestCase $ assertBool "Comma expressions test" $ checkResult (AntaParser.parseString  test4)	
testParse5 = TestCase $ assertBool "Wrong Comma expressions test" $ not $ checkResult (AntaParser.parseString  test5)
testParse6 = TestCase $ assertBool "For inside for" $ checkResult (AntaParser.parseString  test6)
testParse7 = TestCase $ assertBool "Chain of expression with different prioprities operands" $ checkResult (AntaParser.parseString  test7)
testParse8 = TestCase $ assertBool "Lost of whitespaces" $ checkResult (AntaParser.parseString  test8)
testParse9 = TestCase $ assertBool "Right recursion" $ checkResult (AntaParser.parseString  test9)
testParse10 = TestCase $ assertBool "Left recursion" $ checkResult (AntaParser.parseString  test10)
--list of tests
tests = TestList [TestLabel "AntaParser testSuite" $ TestList [testParse1,testParse2, testParse3, testParse4, testParse5, testParse6, testParse7, testParse8, testParse9, testParse10]]

--main
main = do
	runTestTT tests