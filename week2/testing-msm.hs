module Main where
import Test.HUnit
import Data.Either as Either
import MSM

-- check if [] 
emptyStack0 =  [POP, HALT]
emptyStack1 =  [DUP, HALT]
emptyStack2 =  [LOAD, HALT]
emptyStack3 =  [NEG, HALT]
emptyStack4 =  [JMP, HALT]
emptyStack5 =  [CJMP 1, HALT]

-- check if there are less than 2 elem
onlyOneElem0 =  [PUSH 1, ADD, HALT]
onlyOneElem1 =  [PUSH 1, STORE, HALT]
onlyOneElem2 =  [PUSH 1, SWAP, HALT]

-- check register not allocated
noReg =  [PUSH 1, LOAD, HALT]

-- check error on already allocated (NEWREG a)
allocSame =  [NEWREG 1, NEWREG 1, HALT]

-- PC out of bounds
outOfBoundsPC0 =  [PUSH 14, JMP, HALT]
outOfBoundsPC1 =  [PUSH (-4), JMP, HALT]

--Example program, when it terminates it leaves 42 on the top of the stack
p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]
p2  = [NEWREG 0, PUSH 1, DUP, ADD, HALT]


testEmpty0 = TestCase $ assertBool "Stack doesn't have enough elements" $ "StackUnderflow" ==  head(Either.lefts [MSM.runMSM emptyStack0])

testEmpty1 =  TestCase $ assertBool "Stack doesn't have enough elements" $ "StackUnderflow" ==  head(Either.lefts [MSM.runMSM emptyStack1])

testEmpty2 =  TestCase $ assertBool "Stack doesn't have enough elements" $ "StackUnderflow" ==  head(Either.lefts [MSM.runMSM emptyStack2])

testEmpty3 =  TestCase $ assertBool "Stack doesn't have enough elements" $ "StackUnderflow" ==  head(Either.lefts [MSM.runMSM emptyStack4])

testEmpty4 =  TestCase $ assertBool "Stack doesn't have enough elements" $ "StackUnderflow" ==  head(Either.lefts [MSM.runMSM emptyStack4])

testEmpty5 =  TestCase $ assertBool "Stack doesn't have enough elements" $ "StackUnderflow" ==  head(Either.lefts [MSM.runMSM emptyStack5])

testonlyOneElem0 = TestCase $ assertBool "Stack doesn't have enough elements" $ "StackUnderflow" ==  head(Either.lefts [MSM.runMSM onlyOneElem0])

testonlyOneElem1 = TestCase $ assertBool "Stack doesn't have enough elements" $ "StackUnderflow" ==  head(Either.lefts [MSM.runMSM onlyOneElem1])

testonlyOneElem2 = TestCase $ assertBool "Stack doesn't have enough elements" $ "StackUnderflow" == head(Either.lefts [MSM.runMSM onlyOneElem2])

testnoReg = TestCase $ assertBool "Register not allocated" $ "UnallocatedRegister 1" ==  head(Either.lefts [MSM.runMSM noReg])

testallocSame = TestCase $ assertBool "Register already allocated" $ "Register already registered" ==  head(Either.lefts [MSM.runMSM allocSame])

testoutOfBoundsPC0 = TestCase $ assertBool "PC out of bounds" $ "Invalid PC" ==  head(Either.lefts [MSM.runMSM outOfBoundsPC0])

testoutOfBoundsPC1 = TestCase $ assertBool "PC out of bounds" $ "Invalid PC" ==  head(Either.lefts [MSM.runMSM outOfBoundsPC1])


testprogram42 = TestCase $ assertBool "Program 42 works" $ 42 `elem` stack (head(Either.rights [MSM.runMSM p42])) 


tests = TestList [TestLabel "MSM testsuite" $ TestList [testEmpty0,testEmpty1,testEmpty2,testEmpty3,testEmpty4,testEmpty5,testonlyOneElem0,testonlyOneElem1,testonlyOneElem2,testnoReg,testallocSame,testoutOfBoundsPC0,testoutOfBoundsPC1,testprogram42]]


-- main
main = do
	runTestTT tests