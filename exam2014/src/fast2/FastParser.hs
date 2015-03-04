module FastParser
       ( Error
       , parseString
       , parseFile
       )
       where

import FastAST
import Data.Char
import Control.Monad()
import Control.Applicative
import qualified Data.Map as Map
import SimpleParse

program :: Parser Prog
program = classDecls

classDecls :: [Parser ClassDecl]
classDecls = many1 classDecl


classDecl :: Parser ClassDecl
classDecl = do _<- symbol "class"
			   class_name <- name
			   _ <- symbol "{"
			   constructor_declaration <- option constructorDecl
			   named_methods <- nameMethodDecls
			   receive_declaration <- option recvDecl
			   _<- symbol "}"
			   return (ClassDecl {className = class_name,
								  classConstructor = constructor_declaration,
								  classMethods = named_methods,
								  classReceive = receive_declaration } )


constructorDecl :: Parser ConstructorDecl
constructorDecl = do _<- symbol "new"
					 _<- symbol "("
					 parameters <- option params
					 _<- symbol ")"
					 _<- symbol "{"
					 expression <- exprs
					 _<- symbol "}"
					 case parameters of 
						Just p -> return (MethodDecl {methodParameters = p, methodBody = expressions})
						Nothing -> return (MethodDecl {methodParameters = [], methodBody = expressions})





params :: Parser [Name]
params = param `sepBy` (symbol ",")

param :: Parser Name 
param = checkName isLetter

args :: Parser Exprs
args = expr `sepBy` (symbol ",")


exprs : Parser Exprs
exprs = expr `sepBy` (symbol ";")

expr : Parser Expr
expr = return_exp <|> attr_exp
	   where return_exp do _ <- symbol "return"
						   expression <- expr
						   return $ Return expression
						   
attr_exp : Parser Expr
attr_exp = set_self_exp <|> set_exp <|> plus_min
			where set_self_exp = do _ <- symbol "set"
									_ <- symbol "self"
									_ <- symbol "."
									n <- name
									_ <- symbol "="
									expression <- expr
									return $ SetField n expression
				 set_exp = do _ <- symbol "set"
							  n <- name
							  _ <- symbol "="
							  expression <- expr
							  return $ SetVar n Expression
							  
plus_min : Parser Expr
plus_min = mult_div `chainl1` (plus <|> minus) 
				where plus = do _ <- symbol "+"
								return Plus
					  minus = do _ <- symbol "-"
								return Minus
								
mult_div : Parser Expr
mult_div = prim `chainl1` (mult <|> divide)
				where mult = do _ <- symbol "*"
							return Times
					 div = do _ <- symbol "/"
							return DividedBy





prim : Parser Expr
prim = const_int <|> const_string <|> term_with_args <|> term_literal <|> self <|>
	   self_name <|> new_name_args <|> match_exprs <|> send_expr <|> call_methdod <|> simple_exp
	   where const_int = do n <- integer
							return (IntConst n)
			 const_string = do str <- checkString
							return $ StringConst str
			 term_with_args = do s <- name
								 _ <- symbol "("
								 arguments <- args
								 _ <- symbol ")"
								 return $ TermLiteral s arguments
			term_literal = do s <- name
							  _ <- symbol "("
							  _ <- symbol ")"
							  return $ TermLiteral s []
			self = do _ <- symbol "self"
					return $ Self
			self_name = do _ <- symbol "self"
						   _ <- symbol "."
						   n <- name
						   return $ ReadField n
			new_name_args = do _ <- symbol "new"
							   n <- name
							   _ <- symbol "("
							   arguments <- args
							   _ <- symbol ")"
							   return $ New n arguments
			call_method = do expression <- expr
							 _ <- symbol "."
							 n <- name
							 _ <- symbol "("
							 arguments <- args
							 _ <- symbol ")"
							 return $ CallMethod expression n arguments
			match_exprs = do _ <- symbol "match" 
							 expression <- expr
							 _ <- symbol "{"
							 list_of_cases <- cases
							 _ <- symbol "}"
							 return $ Match expression list_of_cases
			send_expr = do _ <- symbol "send"
						   _ <- symbol "("
						   expression <- expr
						   _ <- symbol ","
						   message <- expr
						   _ <- symbol ")"
						   return $ SendMessage expression message
			simple_exp = do _ <- symbol "{"
							expression <- expr
							_ <- symbol "}"
							return expression
				

nameMethodDecls :: Parser [NameMethodDecl]
nameMethodDecls = many1 nameMethodDecl

nameMethodDecl :: Parser NameMethodDecl
nameMethodDecl = do
	method_name <- name
	_ <- symbol "("
	parameters <- params
	_ <- symbol ")"
	_ <- symbol "{"
	expressions <- exprs
	_ <- symbol "}"
	return (NameMethodDecl method_name (MethodDecl { methodParameters = parameters, methodBody = expressions}))
	

recvDecl :: Parser ReceiveDecl
recvDecl = do
	_ <- symbol "receive"
	_ <- symbol "("
	parameter <- param
	_ <- symbol ")"
	_ <- symbol "("
	experssion <- exprs
	_ <- symbol ")"
	return (ReceiveDecl {receiveParam = parameter, receiveBody = expressions})

name :: Parser Name
name = checkName isLetter

checkName :: (Char -> Bool) -> Parser Name
checkName = token (do first <- satisfy p 
					  rest <- many (letter <|> sum <|> undsc)
					  let name1 = first:rest input
						if name1 `notElem` reserved
							then return name1
							else reject)
			where letter = satisfy isLetter
				  num = satisfy isDigit
				  undsc = char '_'

checkString :: parser String
checkString = token (do _ <- symbol "\""
						word <- ascii
						_ <- symbol "\""
						return $ word )

ascii :: Parser String
ascii = many1 ( satisfy isAscii)

integer :: Parser Integer
integer = token (do n <- digits
					return $ read n

digits :: Parser String
digite = many1 (satisfy isDigit)

keyWords :: [String]
keyWords :: ["self","class", "new", "receive", "send", "match", "return", "set"]

reserved :: [String]
reserved :: keywords



-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
type Error = Error deriving (Show,Eq)

parseString :: String -> Either Error Prog
parseString input = case par_result of
					(p, ""):_ -> Right p
					_ -> Left Error
				where par_result = parse (do prog <- program
											 token eof
											 return prog) input

parseFile :: FilePath -> IO (Either Error Prog)
parseFile filename = fmap parseString $ readFile filename
