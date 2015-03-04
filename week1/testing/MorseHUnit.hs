{-
Example code produced for AP lecture.

Using HUnit to test the Morse module

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

import Test.HUnit
import qualified Morse

test1 = TestCase $
        assertBool "Decode Sofia"
        $ "SOFIA" `elem` Morse.decode "...---..-....-"

test2 = TestCase $
        assertBool "Decode Eugenia"
        $ "EUGENIA" `elem` Morse.decode "...---..-....-"

tests = TestList [TestLabel "Decode" $ TestList [test1, test2],
                  TestLabel "Encode" $ "-.-.-." ~=? (Morse.encode "KEN")]

main = runTestTT tests
