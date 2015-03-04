{- 
Example code produced during lecture 4.

Using QuickCheck to test the Morse module.  Second attempt, make a
special generator for strings only containing ASCII letter.  However,
the `listOf` combinator generates strings that are too long to be
practical, here the length is limited.

Date: Sep 11, 2014
Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
import qualified Test.QuickCheck as QC
import qualified Data.Char as C
import qualified Morse

upper = map C.toUpper

prop_encode_decode (LO s) = (upper s) `elem` Morse.decode (Morse.encode s)

main = QC.quickCheck prop_encode_decode

asciiLetter = QC.elements (['a'..'z'] ++ ['A'..'Z'])

newtype LettersOnly = LO String
                    deriving (Eq, Show)

instance QC.Arbitrary LettersOnly where
  arbitrary = fmap LO $ listOf asciiLetter
    
