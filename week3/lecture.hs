{-

E -> int | E + E | E - E

-}

import Control.Applicative
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

runParser :: Parser a -> String -> [(a, String)]
runParser (Parser p) = p

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  [] -> []
  c:cs -> if p c then [(c, cs)] else []

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [ (f x, s')  | (x,s') <- p s ]

instance Monad Parser where
  return x = Parser $ \s -> [(x,s)]
  -- Parser a -> (a -> Parser b) -> Parser b
  p1 >>= f = Parser $ \s -> do
      (x,s') <- runParser p1 s
      let p2 = f x
      (y,s'') <- runParser p2 s'
      return (y,s'')

instance Applicative Parser where
  pure = return
  df <*> dx = do f <- df
                 x <- dx
                 return $ f x

instance Alternative Parser where
  p1 <|> p2 = Parser $ \s -> runParser p1 s ++ runParser p2 s
  empty = Parser $ \_ -> []

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> [((), "")]
  _:_ -> []

--

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
integer = foldl (\acc x -> acc * 10 + x) 0 <$> some digit

finally :: Parser a -> Parser a
finally p = do x <- p
               eof
               return x

data Expr = Con Int | Plus Expr Expr
          deriving Show

expr :: Parser Expr
expr = (Con <$> integer) <|> plusexpr
  where plusexpr = do
          x <- expr
          _ <- satisfy (=='+')
          y <- expr
          return $ Plus x y
