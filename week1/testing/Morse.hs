{-
Model solution for Haskell version of the ruby quiz #121

Mostly a demonstration of how to use list comprehensions for making
succinct solutions.

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module Morse where

import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Char as C

-- | Mapping of ASCII chars to morse code
charMap = [('A',".-"),   ('B',"-..."), ('C',"-.-."), ('D',"-.."), ('E',"."),
           ('F',"..-."), ('G',"--."),  ('H',"...."), ('I',".."),  ('J',".---"),
           ('K',"-.-"),  ('L',".-.."), ('M',"--"),   ('N',"-."),  ('O',"---"),
           ('P',".--."), ('Q',"--.-"), ('R',".-."),  ('S',"..."), ('T',"-"),
           ('U',"..-"),  ('V',"...-"), ('W',".--"),  ('X',"-..-"),('Y',"-.--"),
           ('Z',"--..")]

-- | look up in the char map
findChar :: Char -> String
findChar c = fromMaybe "" $ L.lookup c charMap

-- | encode a string to morse code
encode :: String -> String
encode = concatMap findChar

-- | decode morse code into all possible decodings
decode :: String -> [String]
decode ""    = [""]
decode input = [ letter : rest | (letter, code) <- charMap
                               , code `L.isPrefixOf` input
                               , let clength = length code
                               , rest <- decode $ drop clength input]
