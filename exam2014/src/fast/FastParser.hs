module FastParser
       ( Error
       , parseString
       , parseFile
       )
       where

import FastAST
import Text.ParserCombinators.ReadP

-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
data Error a = NoParses
			| Remainder (a,String)
			| Ambiguous [(a,String)]
			deriving (Show, Eq)
	

	
	
----named declarations	
--pReceiveDecl :: ReadP ReceiveDecl
--pReceiveDecl = do 
--				_ <- string "receive"
--				_ <- char '('
--				param <- pParam
--				_ <- char ')'
--				_ <- char '{'
--				exprs <- pExprs 
--				_ <- char '}'
--				return $ ReceiveDecl {param,exprs}
--				
--pNamedMethodDecl :: ReadP NamedMethodDecl
--pNamedMethodDecl = do
--				name <- pName
--				_ <- char '('
--				params <- pParams
--				_ <- char ')'
--				_ <- char '{'
--				exprs <- pExprs 
--				_ <- char '}'
--				return $ NamedMethodDecl name $ params exprs
--				
--pNamedMethodDecls :: ReadP [pNamedMethodDecl] 	
--pNamedMethodDecls = do
--					namedMethodDecl <- pNamedMethodDecl
--					pNamedMethodDeclsTail namedMethodDecl
--				
--pNamedMethodDeclsTail :: NamedMethodDecl -> Readp [NamedMethodDecl]	
--pNamedMethodDeclsTail namedMethodDecl = return [NamedMethodDecl] +++
--											pNamedMethodDecls
--											
--
--				
--			
--			
--parameters
pParams :: ReadP [Name]
pParams = do
		  name <- pName
		  pParamsTail [name]
		  
pParamsTail :: [Name] -> ReadP [Name]
pParamsTail names = return names +++ pParams2 names

pParams2 ::[Name] -> ReadP [Name]
pParams2 names = do
			_ <- char ','
			name <- pName
			pParamsTail (names ++ [name])
			
pParam :: ReadP Name
pParam = do 
		name <- pName
		return $ name

----expressions
--pExprs :: ReadP Exprs
--pExprs = do
--		expr <- pExpr
--		pExprsTail expr
--		
--pParamsTail :: Expr -> ReadP Exprs
--pParamsTail name = return [name] +++ pExprs
--
--arguments
pArgs :: ReadP [Expr]
pArgs = do
		  expr <- pExpr
		  pArgsTail [expr]
		  
pArgsTail :: [Expr] -> ReadP [Expr]
pArgsTail exprs = return exprs +++ pArgs2 exprs

pArgs2 ::[Expr] -> ReadP [Expr]
pArgs2 exprs = do
			_ <- char ','
			expr <- pExpr
			pArgsTail (exprs ++ [expr])

--exprs
pExprs :: ReadP Exprs
pExprs = do
		  expr <- pExpr
		  pExprsTail $ [expr]
		  
pExprsTail :: Exprs -> ReadP Exprs
pExprsTail exprs = return exprs +++ pExprs2 exprs

pExprs2 :: Exprs -> ReadP Exprs
pExprs2 exprs = do
			_ <- char ';'
			expr <- pExpr
			pExprsTail ([expr] ++ exprs)

			
--expression
pExpr :: ReadP Expr
pExpr = pTermLiteral <++
		pReturn <++
		pIntConst <++
		pStringConst  +++
			pSelf   +++
			pSetField  +++
			pSetVar  -- +++
--				pPlus +++
--				pMinus +++
--				pTimes +++
--				pDividedBy  -- +++
--					pMatch +++
--					pSendMessage +++
--					pSelfName +++
--					pCallMethod +++
--					pNew ++
--					pBracketsExpr
					
											

pIntConst :: ReadP Expr
pIntConst = do
			integer <- munch1 (`elem` ['0'..'9'])
			return $ IntConst $ read integer
			
pStringConst :: ReadP Expr
pStringConst = do
			   str <- pString
			   if  isReserved str then pfail else return $ StringConst str
			   

			   
pTermLiteral :: ReadP Expr
pTermLiteral = do
				name <- pName 
				_ <- char '('
				args <- pArgs
				_ <- char ')'
				return $ TermLiteral name args
				
pSelf :: ReadP Expr
pSelf = do
			_ <- string "self"
			return $ Self
			
pReturn :: ReadP Expr
pReturn = do 
			_ <- string "return"
			expr <- pExpr
			return $ Return expr
			
	
pSetField :: ReadP Expr
pSetField = do 
			_ <- string "set"
			_ <- string "self"
			_ <- char '.'
			name <- pName
			_ <- char '='
			expr <- pExpr 
			return $ SetField name expr
			
pSetVar :: ReadP Expr 
pSetVar = do 
		_ <- string "set"
		name <- pName
		_ <- char '='
		expr <- pExpr 
		return $ SetVar name expr
		
--pPlus :: ReadP Expr 
--pPlus = do 
--		exp1 <- pExpr
--		_ <- char '+'
--		exp2 <- pExpr
--		pExpr $ Plus exp1 exp2
--	
--pMinus :: ReadP Expr 
--pMinus = do 
--		exp1 <- pExpr
--		_ <- char '-'
--		exp2 <- pExpr
--		return $ Minus exp1 exp2
--		
--
--pTimes :: ReadP Expr 
--pTimes = do 
--		exp1 <- pExpr
--		_ <- char '*'
--		exp2 <- pExpr
--		return $ Times exp1 exp2
--
--pDividedBy :: ReadP Expr 
--pDividedBy = do 
--		exp1 <- pExpr
--		_ <- char '/'
--		exp2 <- pExpr
--		return $ DividedBy exp1 exp2
		
--plusOp :: ReadP (Expr -> Expr -> Expr)
--plusOp = do symbol "+"
--           return exprPlus	
--		   
--minusOp :: ReadP (Expr -> Expr -> Expr)
--minusOp = do symbol "-"
--           return exprMinus	
--		   
--timesOp :: ReadP (Expr -> Expr -> Expr)
--timesOp = do symbol "*"
--           return exprTimes
--		   
--dividedByOp :: ReadP (Expr -> Expr -> Expr)
--dividedByOp = do symbol "/"
--				return exprDividedBy
--		   
	
--pMatch :: ReadP Expr
--pMatch = do
--		_ <- string "match"
--		expr <- pExpr
--		_ <- char '{'
--		c <- pCases
--		_ <- char '}'
--		return $ Match expr c
--
--pSendMessage :: ReadP Expr
--pSendMessage = do
--			_ < string "send"
--			_ <- char '('
--			expr1 <- pExpr
--			_ <- char ','
--			expr2 <- pExpr
--			_ <- char ')'
--			return $ SendMessage expr1 expr2
--			
--pSelfName :: ReadP Expr
--pSelfName = do
--			_ <- string "self"
--			_ <- char '.'
--			name <- pName
--			return $ StringConst name
--			
--pCallMethod	:: ReadP Expr
--pCallMethod = do 
--			expr <- pExpr
--			_ <- '.'
--			name <- pName
--			_ <- char '('
--			args <- pArgs
--			_ <- char ')'
--			return $ CallMethod expr name args

--pNew :: ReadP Expr
--pNew = do 
--		_ <- string "new"
--		name <- pName
--		_ <- char '('
--		args <- pArgs
--		_ <- char ')'
--		return $ New name args
--		
--pBracketsExpr :: ReadP Expr
--pBracketsExpr = do
--		_ <- '('
--		expr <- pExpr
--		_ <- ')'
--		return expr
--
--		
----cases			
--pCases :: ReadP Cases
--pCases = pEmptySpaces +++ pCases'
--
--pEmptySpaces :: ReadP Cases
--pEmptySpaces = do
--				_ <- char ' '
--				return $ []
			
--pCases' :: ReadP Cases
--pCases' = do
--		  c <- pCase 
--		  pCasesTail c
--		  
--pCasesTail :: Case -> ReadP [Case]
--pCasesTail c= return [c] ++ pCases'			
----case			
--pCase :: ReadP Case
--pCase = do 
--		pattern <- pPattern
--		_ <- string "->"
--		_ <- char '{'
--		exprs <- pExprs
--		_ <- char '}'
--		return $(pattern, exprs)
--			
--			
--pattern
pPattern :: ReadP Pattern
pPattern = pConstInt +++
				pConstString +++
					pTermPattern +++
						pAnyValue
						
pConstInt :: ReadP Pattern
pConstInt = do 
			integer <- munch1 (`elem` ['0'..'9'])
			return $ ConstInt $ read integer
			
pConstString :: ReadP Pattern
pConstString = do
			   c <- munch1 (`elem` ['0'..'9'] ++ ['_','&','*',';','.'])
			   str <- pString
			   let string = c ++ str
			   return $ ConstString string
			   
pTermPattern :: ReadP Pattern
pTermPattern = do
			   name <- pName
			   _ <- char '('
			   names <- pParams
			   _ <- char ')'
			   return $ TermPattern name names
			   
pAnyValue :: ReadP Pattern
pAnyValue = do
			name <- pName
			return $ AnyValue name
			
			
--name			
pName :: ReadP Name
pName = do
		ch <- munch1 (`elem` ['A'..'Z']++['a'..'z'])
		chx <- munch (`elem` ['A'..'Z']++['a'..'z']++['_'] ++ ['0'..'9'])
		let name = ch ++ chx
		if  isReserved name then pfail else return name 
					
reservedWords :: [String]
reservedWords = ["self","class","new","receive","send","match","return","set"]

isReserved :: String -> Bool
isReserved s = s `elem` reservedWords



		
pString :: ReadP String
pString = do 
		 str <- munch1 (`elem` ['A'..'Z']++['a'..'z']++['_','&','*',';','.','(',')'] ++ ['0'..'9']) 
		 return $ str
		 

		 

--parseString :: String -> Either Error Prog
--parseString = undefined
-- parseString (eofWrap pSIdents) "gdfhw"
eofWrap :: ReadP a -> ReadP a
eofWrap p = do
	r <- p
	eof
	
	return r

parseString :: ReadP a -> String -> Either (Error a) a
parseString p s = case(readP_to_S p s) of 
					[(r,"")] -> Right r
					[] -> Left $ NoParses
					[(r,rem)] -> Left $ Remainder (r,rem)
					parses -> Left $ Ambiguous parses

parseFile :: FilePath -> IO (Either (Error a) Prog)
parseFile = undefined
