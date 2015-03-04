import Data.List
import Data.Maybe
import Data.Char

encode :: String -> String
encode x = concatMap encodeChar x

myLetters = [('a', ".-")
  ,('b', "-...")
  ,('c', "-.-.")
  ,('d', "-..")
  ,('e', ".")
  ,('f', "..-.")
  ,('g', "--.")
  ,('h', "....")
  ,('i', "..")
  ,('j', ".---")
  ,('k', "-.-")
  ,('l', ".-..")
  ,('m', "--")
  ,('n', "-.")
  ,('o', "---")
  ,('p', ".--.")
  ,('q', "--.-")
  ,('r', ".-.")
  ,('s', "...")
  ,('t', "-")
  ,('u', "..-")
  ,('v', "...-")
  ,('w', ".--")
  ,('x', "-..-")
  ,('y', "-.--")
  ,('z', "--..")]
  
myMorse = [(".-",'a')
  ,("-...", 'b')
  ,("-.-.", 'c')
  ,("-..", 'd')
  ,(".",'e')
  ,("..-.", 'f')
  ,("--.", 'g')
  ,("....",'h')
  ,("..", 'i')
  ,(".---",'j')
  ,("-.-",'k')
  ,(".-..", 'l')
  ,("--", 'm')
  ,("-.", 'n')
  ,("---", 'o')
  ,(".--.", 'p')
  ,("--.-", 'q')
  ,(".-.", 'r')
  ,("...", 's')
  ,("-", 't')
  ,("..-", 'u')
  ,("...-", 'v')
  ,(".--", 'w')
  ,("-..-",'x')
  ,("-.--", 'y')
  ,("--..", 'z')]
  
encodeChar :: Char -> String
encodeChar x = fromJust (lookup x myLetters)

{-decode :: String -> String
decode x = concatMap decodeList x

decodeList :: String -> String
decodeList x = fromJust (lookup x myMorse)-}
	