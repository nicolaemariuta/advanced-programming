import Test.QuickCheck
import qualified Morse
import qualified Data.Char as C


upper = map C.toUpper
good = all $ \c -> C.isAscii c && C.isLetter c

prop_encode_decode s = good s ==> upper s `elem` Morse.decode (Morse.encode s)

main = quickCheck prop_encode_decode


-- version including classification of input
-- prop_encode_decode s = classify (null s) "empty string" $
--                            good s ==> upper s `elem` Morse.decode (Morse.encode s)
