{-
Example code produced for AP lecture.

Using QuickCheck to test the Morse module.  Third attempt, make a
special generator for strings only containing (uppercase) ASCII
letters, with length one to five. Letters are selected with frequency
inverse to the length of their morse code encoding (because we have a
performance problem in Morse.decode.)

Main purpose is to demonstrate how to use `frequency`.

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

import Test.QuickCheck
import qualified Data.Char as C
import qualified Morse

upper = map C.toUpper
lower = map C.toLower

prop_encode_decode (LO s) = upper s `elem` Morse.decode (Morse.encode s)


weightedLetters = frequency [(2^(max - length code), return c)
                            | (c,code) <- Morse.charMap]
  where max = 1 + (maximum $ map (length . snd) Morse.charMap)

newtype LettersOnly = LO String
                    deriving (Eq, Show)

instance Arbitrary LettersOnly where
  
  arbitrary = fmap LO $ do n <- choose (0, 5)  -- choose n between 1 and 5
                           vectorOf n weightedLetters
