{-# OPTIONS_GHC -Wall #-}
module AntaParser
       ( Error
       , parseString
       , parseFile
       )
       where

import SimpleParse
import Data.Char
import Control.Applicative
import AntaAST

--Error type
type Error = ErrorType

data ErrorType = Unspec String | Ambiguous String
	deriving (Show)

entry :: Parser Program
entry = prog <* spaces

--keywords 
keywords :: [String]
keywords = ["True", "False", "range", "for", "if", "in", "not"]

-- parser for Name
name :: Parser Name
name = token $ do
				name_start <- satisfy nameStart
				name_rest <- munch nameRest
				if (name_start : name_rest) `elem` keywords
					then reject
				else return (name_start : name_rest)
-- names must start with a letter
nameStart :: Char -> Bool
nameStart c = isAlpha c
-- names can include letters, digits and underscores
nameRest :: Char -> Bool
nameRest c = isAlpha c || c == '_' || isDigit c

--parse for integers. It is returned as Expr
intConst :: Parser Integer
intConst = token $
	do
		integer <- munch1 isDigit
		return $ read integer


--parsing of main program structures		
prog :: Parser Program
prog = decls

decls :: Parser Decls
decls = many decl 

decl :: Parser Decl
decl = do 
		decl_name <- name
		_ <- symbol "="
		decl_expr <- expr
		return (decl_name, decl_expr)
			

args3 :: Parser Args3
args3 = 
	do 
		e1 <- expr
		return $ A1 e1
	<|>
	do
		e1 <- expr
		_ <- symbol ","
		e2 <- expr
		return $ A2 e1 e2
	<|>
	do
		e1 <- expr
		_  <- symbol ","
		e2 <- expr		
		_  <- symbol ","
		e3<- expr
		return $ A3 e1 e2 e3
		
		
exprs :: Parser Exprs
exprs = 
	do
		es <- commaExprs
		return es
			
commaExprs :: Parser [Expr]
commaExprs = 
	do
		y <- (expr `sepBy` (symbol ","))
		return y
	
--parsing of expression	
expr :: Parser Expr
expr = 
	do
		e <- exp0
		return e

exp0 :: Parser Expr
exp0 =
	do
		e1 <- exp1
		eop0 e1



--parsing expr operations
eop0 :: Expr -> Parser Expr
eop0 e = 
	do 
		_ <- symbol "in"
		e2 <- exp1
		er <- eop0 e2
		return $ In e er
	<|>
	do
		_ <- symbol "not"
		_ <- symbol "in"
		e2 <- exp1
		er <- eop0 e2
		return $  NotIn e er
	<|>
		return e
		

exp1 :: Parser Expr
exp1 =
	do
		e1 <- exp2
		eop1 e1
		
--right recursion for oprator ==
eop1 :: Expr -> Parser Expr 
eop1 e =
	do
		_ <- symbol "=="
		e2 <- exp2
		er <- eop1 e2
		return $ Equal e er
	<|>
		return e
	
exp2 :: Parser Expr
exp2 = 
	do
		e3 <- exp3
		eop2 e3
        
--parsing expressions Plus and Minus
eop2 :: Expr -> Parser Expr
eop2 e = 
	do
		_ <- symbol "+"
		e3 <- exp3
		eop2 $ Plus e e3
	<|>
	do
		_ <- symbol "-"
		e3 <- exp3
		eop2 $ Minus e e3
	<|>
		return e

exp3 :: Parser Expr
exp3 =
	do
		e4 <- exp4
		eop3 e4		
		
eop3 :: Expr -> Parser Expr
eop3 e = 
	do
		_ <- symbol "*"
		e4 <- exp4
		eop3 $ Mult e e4
	<|>
	do
		_ <- symbol "//"
		e4 <- exp4
		eop3 $ Div e e4
	<|>
	do
		_ <- symbol "%"
		e4 <- exp4
		eop3 $ Modulus e e4
	<|>
		return e
		
	
exp4 :: Parser Expr
exp4 = 
	do 
		c1 <- intConst 
		return $ IntConst c1
	<|>
	do
		_ <- symbol "True"
		return TrueConst
	<|>
	do	
		_ <- symbol "False"
		return FalseConst
	<|>
	do 
		n1 <- name
		return $ Name n1
	<|>
	do
		_ <- symbol "range"
		_ <- symbol "("
		a3 <- args3
		_ <- symbol ")"
		return $ Range a3
	<|>
	do
		_ <- symbol "["
		l1 <- listComp
		_ <- symbol "]"
		return $ ListComp l1
	<|>
	do
		_ <- symbol "("
		e1 <- expr
		_ <- symbol ")"
		return e1
		
	
		

		


--parsing of the lists data structures		
listComp :: Parser ListComp
listComp = 
	do
		l1 <-  exprs
		return $ Simple l1
	<|>
	do
		l1 <- expr
		l2 <- listFor
		return $ ListFor l1 l2
--parsing  for		
listFor :: Parser ListFor
listFor	= 
	do
		_ <- symbol "for"
		n1 <- name
		_ <- symbol "in"
		e1 <- expr
		l1 <- option listIter
		return (n1,e1, l1)
        
--parsing inter		
listIter :: Parser ListIter
listIter = 
	do
		l1 <-  pListIter
		return l1
		
pListIter :: Parser ListIter
pListIter =
	do
		l1 <- listFor
		return $ ListForIter l1
	<|>
	do
		_ <- symbol "if"
		e1 <- expr
		l1 <-  option listIter
		return $ ListIf e1 $ l1


--utilty functions for starting the parsing
parseString :: String -> Either Error Program
parseString s = checkError (parseEof entry s)

checkError :: [(Program, String)] -> Either Error Program
checkError ((p, _):[]) = Right p
checkError ((_, _):_) = Left (Ambiguous "Could you specify that?")
checkError [] = Left (Unspec "Fatal parsing error")

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = parseString <$> readFile filename


