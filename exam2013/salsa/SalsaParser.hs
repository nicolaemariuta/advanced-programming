--
-- Skeleton for Salsa parser
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaParser where

import SalsaAst
import Text.ParserCombinators.ReadP

data Error a = NoParses
			| Remainder (a,String)
			| Ambiguous [(a,String)]
			deriving (Show, Eq)

pColour :: ReadP Colour
pColour = pColour' "blue" Blue +++
		 pColour' "plum" Green +++
		 pColour' "red" Red +++
		 pColour' "green" Green +++
		 pColour' "orange" Orange 
		 
pColour' :: String -> Colour -> ReadP Colour
pColour' s c = (string s) >> (return c)

pExpr :: ReadP Expr 
pExpr =	do
		prim <- pPrim
		pExpreTail prim
		
pSIdents :: ReadP [Ident]
pSIdents =	do
		 x <- pSIdent
		 pSIdentTail x

pSIdentTail :: Ident -> ReadP [Ident]
pSIdentTail idents = return [idents] +++
					pSIdents
					
pVIdents :: ReadP [Ident]
pVIdents =	do
		 x <- pVIdent
		 pVIdentTail x

pVIdentTail :: Ident -> ReadP [Ident]
pVIdentTail idents = return [idents] +++
					pVIdents
					
					
			
pExpreTail :: Expr -> ReadP Expr
pExpreTail expr = return expr +++
				  pExpreTailPlus expr +++
				  pExpreTailMinus expr
			 
pExpreTailPlus ::Expr -> ReadP Expr
pExpreTailPlus expr1 = do
		_ <- char '+'
		expr2 <- pPrim
		expr3 <- pExpreTail expr2
		return (Plus expr1 expr3)
		
pExpreTailMinus ::Expr -> ReadP Expr
pExpreTailMinus expr1 = do
		_ <- char '-'
		expr2 <- pPrim
		expr3 <- pExpreTail expr2
		return (Minus expr1 expr3)
		
pVIdent :: ReadP Ident
pVIdent = do
	c <- satisfy (`elem` ['A'..'Z'])
	cs <- munch (`elem` ['A'..'Z']++['a'..'z']++['_'] ++ ['0'..'9']) 
	return $ c:cs


pSIdent :: ReadP Ident
pSIdent = do
	c <- satisfy (`elem` ['a'..'z'])
	cs <- munch (`elem` ['A'..'Z']++['a'..'z']++['_'] ++ ['0'..'9']) 
	return $ c:cs
	
pInteger :: ReadP Expr
pInteger = do munch (`elem` ['A'..'Z']++['a'..'z']++['_'] ++ ['0'..'9'])
	digits <- munch (`elem` ['0'..'9'])
	return $ Const $ read digits
	
pPrim :: ReadP Expr
pPrim = pInteger +++
		pPrimExpr +++
		pPrimIdentx +++
		pPrimIdenty
		
pPrimIdentx :: ReadP Expr
pPrimIdentx = do
		ident <- pSIdent 
		_<-string ".x"
		return $ Xproj ident
	
pPrimIdenty :: ReadP Expr
pPrimIdenty = do
		ident <- pSIdent 
		_<- string ".y"
		return $ Yproj ident	

sstring :: ReadP a -> ReadP a
sstring p = do
	skipSpaces
	r <- p
	skipSpaces
	return r
		
pPrimExpr :: ReadP Expr
pPrimExpr =	do
		c<- satisfy (`elem` "(")
		expr <- pExpr
		d <- satisfy (`elem` ")")
		return expr

eofWrap :: ReadP a -> ReadP a
eofWrap p = do
	r <- p
	eof
	return r

pPos :: ReadP Pos
pPos = pPosAbs +++
		pPosRel
		
pPosAbs :: ReadP Pos
pPosAbs = do
	_ <- char '('
	expr1 <- pExpr
	_ <- char ','
	expr2 <- pExpr
	_ <- char ')'
	return $ Abs expr1 expr2
	
pPosRel :: ReadP Pos
pPosRel = do
	_ <- char '+'
	_ <- char '('
	expr1 <- pExpr
	_ <- char ','
	expr2 <- pExpr
	_ <- char ')'
	return $ Rel expr1 expr2
	

	
pCommand :: ReadP Command
pCommand = do
			_ <- string "||"
			command <- pCommand
			return $ Par $ pCommandTail command

pCommand1 +++
			pCommand2 +++
			 pCommand3
			 
pCommand1 :: ReadP Command
pCommand1 = do
			sIdents <- pSIdents
			_ <- string "->"
			pos <- pPos
			pCommandTail' $ Move sIdents pos

token :: String -> ReadP ()
token s =
	skipSpaces
	string s
	skipSpaces

pCommand2 :: ReadP Command
pCommand2 = do
			token "{"
			comm <- pCommand
			token "}"
			pCommandTail' comm
			
pCommandTail :: Command -> ReadP Command
pCommandTail command = return command +++
							pCommandTail1 command  
							 
pCommandTail1 :: Command -> ReadP Command
pCommandTail1 command = do
					_ <- char '@'
					vident <- pVIdent
					pCommandTail' $ At command vident  

pCommandTail' :: Command -> ReadP Command
pCommandTail' command = return command +++
							pCommandTail1 command +++
								pCommandTail2 command 					

pCommandTail2 :: Command -> ReadP Command
pCommandTail2 command = do
					_ <- string "||"
					command2 <- pCommand
					pCommandTail' $ Par command command2  
					
pDefinition :: ReadP Definition
pDefinition = pDef1 +++
				pDef2 +++
					pDef3 +++
						pDef4 +++
							pDef5
							
pDef1 :: ReadP Definition
pDef1 = do
			_ <- string "viewdef"
			vident <- pVIdent
			expr1 <- pExpr
			expr2 <- pExpr
			return $ Viewdef vident expr1 expr2
			
pDef2 :: ReadP Definition
pDef2 = do
			_ <- string "rectangle"
			sident <- pSIdent
			expr1 <- pExpr
			expr2 <- pExpr
			expr3 <- pExpr
			expr4 <- pExpr
			colour <- pColour
			return $ Rectangle sident expr1 expr2 expr3 expr4 colour	 		

pDef3 = do
			_ <- string "circle"
			sident <- pSIdent
			expr1 <- pExpr
			expr2 <- pExpr
			expr3 <- pExpr
			colour <- pColour
			return $ Circle sident expr1 expr2 expr3 colour

pDef4 :: ReadP Definition
pDef4 = do
			_ <- string "view"
			vident <- pVIdent
			return $ View vident
			
pDef5 :: ReadP Definition
pDef5 = do
			_ <- string "group"
			vident <- pVIdent
			_ <- char '['
			vidents <- pVIdents
			_ <- char ']'
			return $ Group vident vidents

pDefCom :: ReadP DefCom
pDefCom = pDefCom1 +++ pDefCom2

pDefCom1 :: ReadP DefCom
pDefCom1 = do 
			command <- pCommand
			return $ Com command
			
 		
pDefCom2 :: ReadP DefCom
pDefCom2 = do 
			def <- pDefinition
			return $ Def def

pDefComs :: ReadP [DefCom]	
pDefComs = do
			defcom <- pDefCom
			pDefComsTail defcom
			
pDefComsTail :: DefCom -> ReadP [DefCom] 
pDefComsTail defcoms = return [defcoms] +++
						pDefComs 
				
		
parseString :: ReadP a -> String -> Either (Error a) a
parseString p s = case(readP_to_S p s) of 
					[(r,"")] -> Right r
					[] -> Left $ NoParses
					[(r,rem)] -> Left $ Remainder (r,rem)
					parses -> Left $ Ambiguous parses


--parseFile :: FilePath -> IO (Either Error Program)
