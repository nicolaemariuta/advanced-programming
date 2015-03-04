import Test.HUnit
import qualified MSM
test1 = TestCase $ assertBool "Init MSM" $[NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]  MSM.initial Stack 42
tests = TestList [TestLabel "MSM" $ TestList [test1]]
main = runTestTT tests
