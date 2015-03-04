newtype Parser a = ...

instance Monad Parser where
	(>>=) :: Parser a -> (a -> Parser b) -> Parser b
	return a -> Parser a
instance MonadPlus Parser where
	mzero :: Parser a
	mplus :: Parser a -> Parser a -> Parser a
	
	

runParser :: Parser a -> String -> Either Error a

reject :: Parser a
char :: Char -> Parser Char
string :: String -> Parser String
satisfy :: (Char -> Bool) -> Parser Char

eof :: Parser ()

(<|>) :: Parser a -> Parser a -> Parser a

many :: Parser a -> Parser [a]
many1 :: Parser a -> Parser [a]

option :: Parser a -> Parser (Maybe a)

-- set of combinators to deal the lexical analysis

space :: Parser Char
space = satisfy isSpace
spaces :: Parser String 
spaces = many space
spaces1 :: Parser String
spaces1 = many1 space

token :: Parser a -> Parser a
token p = spaces >> p

symbol :: String -> Parser String
symbol = token . string
schar :: Char -> Parser Char
schar = token . char

-- scanning function for names 
name :: Parser String
name = token (do c <- letter
				 cs <- letdigs
				 return (c:cs))
		where letter = satisfy isLetter
			  letdigs = many (letter <|> num)
			  num = satisfy isDigit   --used token combinator to skip spaces before names
			  
--extension to distinguish keywords from names
data Keyword = Keyword String

reserved :: [String]
reserved = ["class","data","type"]

nameOrKeyword :: Parser (Either Keyword String)
nameOrKeyword = do n <-name
				   if n `elem` reserved then return (Left(Keyword n))
				   else return (Right n)
				   
--parser for paring floating point numbers
realNumber :: Parser Double
realNumber = token (do pre <- digits
					   char '.'
					   post <- digits
					   reutrn $ read $ pre ++ "." ++ post)
					 where digits = many1(satisfy isDigit)
					 
--syntax trees
data Expr = Zeroterm
		| Oneterm
		| Minus Expr Expr
		deriving (Eq, Show)
		
-- parser for mathematical expressions
e = do t
	   eopt
	   return()
 eopt =(do symbol "-"
			t
			eopt
			return ())
	<|> (do symbol "+"
			t 
			eopt 
			return ())
	 <|> return () 
 t = do f 
		topt 
		return ()
 topt = (do symbol "*"
			f 
			eopt
			return ())
	<|> (do symbol "/"
			f
			topt 
			return ())
	 <|> return ()
 f = (do _ <- realNumber
		 return ())
	<|> (do symbol "("
			e
			symbol ")"
			return())
 parseString = r >> token eof

-- extend to evaluate arithmetic expressions
e, t, f ::: Parser Double 
e = do tv <- t 
	   eopt tv 
eopt :: Double -> Parser Double
eopt inval =
			(do symbol "-"
				tv <- t 
				eopt(inval - tv))
		   <|> (do symbol "+"
				   tv <- t 
				   eopt(inval + tv))
		   <|> return inval
	t = do fv <- f
		topt fv
	topt :: Double -> Parser Double
	topt inval =
			(do symbol "*"
			fv <- f
			topt(inval * fv))
		<|> (do symbol "/"
			fv <- f
			topt(inval / fv))
		<|> return inval
	f = realNumber
		<|> (do symbol "("
			ev <- e
			symbol ")"
			return ev)
