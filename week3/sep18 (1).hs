{-

E -> int | E + E | E - E

-}

import SimpleParse
import Control.Applicative
import Data.Char

anyChar :: Parser Char
anyChar = satisfy $ \_ -> True

digit :: Parser Int
digit = num <$> satisfy isDigit
  where num c = ord c - ord '0'

digitOrChar :: Parser (Either Int Char)
digitOrChar = lhs <|> rhs
  where lhs = Left <$> digit
        rhs = Right <$> anyChar

integer :: Parser Int
integer = token $ foldl (\acc x -> acc * 10 + x) 0 <$> some digit

finally :: Parser a -> Parser a
finally p = do x <- p
               eof
               return x

data Exp = Con Int | Add Exp Exp | Sub Exp Exp
         | Mul Exp Exp | Div Exp Exp
         | Var String | If Exp Exp Exp
         deriving (Show)

entry :: Parser Exp
entry = e0 <* spaces

e0 :: Parser Exp
e0 = chainl1 e1 op0

e1 :: Parser Exp
e1 = chainl1 t op1

keywords :: [String]
keywords = ["if", "then", "else"]

variable :: Parser Exp
variable = token $ do s <- munch1 constituent
                      if s `elem` keywords
                        then reject
                        else return $ Var s

constituent :: Char -> Bool
constituent c = isAlpha c || c == '_'

keyword :: String -> Parser ()
keyword s = do _ <- symbol s
               notFollowedBy $ satisfy constituent

ifExp :: Parser Exp
ifExp = do keyword "if"
           x <- e0
           keyword "then"
           y <- e0
           keyword "else"
           z <- e0
           return $ If x y z

t :: Parser Exp
t = do x <- integer
       return (Con x)
    <|> do _ <- symbol "("
           x <- e0
           _ <- symbol ")"
           return x
    <|> ifExp
    <|> variable

op0 :: Parser (Exp -> Exp -> Exp)
op0 = (do _ <- symbol "+"
          return Add) <|>
      (do _ <- symbol "-"
          return Sub)

op1 :: Parser (Exp -> Exp -> Exp)
op1 = (do _ <- symbol "/"
          return Div) <|>
      (do _ <- symbol "*"
          return Mul)
