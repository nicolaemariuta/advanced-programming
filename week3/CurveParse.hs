module CurveParse where

import CurveAST
import Text.ParserCombinators.ReadP

data Error a = NoParses
			| Remainder (a,String)
			| Ambiguous [(a,String)]
			deriving (Show, Eq)

pExpr :: ReadP Expr
pExpr = pExpr1 +++
			pExpr2 +++
				pExpr3 +++
					pExpr4 +++
						pExpr5 +++
							pExpr6
							
pExpr1 :: ReadP Expr
pExpr1 = do
		  expr1 <- pExpr
		  _  <- char '+'
		  expr2 <- pExpr
		  pExprTail $ Add expr1 expr2

pExpr2 :: ReadP Expr
pExpr2 =  do
		  expr1 <- pExpr
		  _  <- char '*'
		  expr2 <- pExpr
		  pExprTail $ Mult expr1 expr2		  

pExpr3 :: ReadP Expr
pExpr3 =  do
		  _  <- string "width"
		  curve <- pCurve
		  return $ Width curve	

pExpr4 :: ReadP Expr
pExpr4 =  do
		  _  <- string "height"
		  curve <- pCurve
		  return $ Height curve			  
pExpr5 :: ReadP Expr
pExpr5 = do
		  number  <- pNumber
		  return $ Const number	
		  
pExpr6 :: ReadP Expr
pExpr6 =  do
		  _ <- char '('
		  expr   <- pExpr
		  _ <- char ')'
		  return $ pExprTail expr

pExprTail :: Expr -> ReadP Expr
pExprTail expr = return expr +++
					pExpr expr
					
pPoint :: ReadP Point
pPoint = do
		_ <- char '('
		expr1 <- pExpr
		_ <- char ','
		expr2 <- pExpr
		_ <- char ')'
		return $ Point expr1 expr2
		
pCurve :: ReadP Curve
pCurve = pCurve1 +++
			pCurve2 +++
				pCurve3 +++
					pCurve4 +++
						pCurve5 +++
							pCurve6 +++
								pCurve7 +++
									pCurve8 +++
										pCurve9 +++
											pCurve10
											
pCurveTail :: Curve -> ReadP Curve
pCurveTail curve = return Curve +++
						pCurve curve
						
pCurve1 :: ReadP Curve
pCurve1 =  do
			curve1 = pCurve
			_ <- string "++"
			curve2 = pCurve
			pCurveTail $ Connect curve1 curve2
			
pCurve2 :: ReadP Curve
pCurve2 =  do
			curve1 = pCurve
			_ <- char '^'
			curve2 = pCurve
			pCurveTail $ Over curve1 curve2
			
pCurve3 :: ReadP Curve
pCurve3 =  do
			curve = pCurve
			_ <- string "->"
			point = pPoint
			pCurveTail $ Translate curve point
			
pCurve4 :: ReadP Curve
pCurve4 =  do
			curve = pCurve
			_ <- string "**"
			expr = pExpr
			pCurveTail $ Scale curve expr
					
pCurve5 :: ReadP Curve
pCurve5 =  do
			curve = pCurve
			_ <- string "refv"
			expr = pExpr
			pCurveTail $ Refv curve expr
			
pCurve6 :: ReadP Curve
pCurve6 =  do
			curve = pCurve
			_ <- string "refh"
			expr = pExpr
			pCurveTail $ Refh curve expr
			
pCurve7 :: ReadP Curve
pCurve7 =  do
			curve = pCurve
			_ <- string "rot"
			expr = pExpr
			pCurveTail $ Rot curve expr
			
pCurve8 :: ReadP Curve
pCurve8 =  do
			_ <- char '('
			curve <- pCurve
			_ <- char ')'
			pCurveTail $ curve

pCurve9 :: ReadP Curve
pCurve9 =  do
			point <- pPoint
			pCurveTail $ Single Point

pCurve10 :: ReadP Curve
pCurve10 =  do
			ident <- pIdent
			pCurveTail $ Id ident	

pDef :: ReadP Def
pDef = pDef1 +++
		pDef2
		
pDef1 :: ReadP Def
			
			
pIdent :: ReadP Ident  --sa nu fie din cuvintele rezervate
pIdent = do
		ident <- munch (`elem` ['A'..'Z']++['a'..'z']++['_'] ++ ['0'..'9'])
		return $ ident
			
pNumber :: ReadP Number
pNumber = do
		xs <- munch(`elem` ['0'..'9'])
		char '.'
		ys <- munch(`elem` ['0'..'9'])
		let digits = xs ++ ('.':ys)
		return $ read digits

parseString :: ReadP a -> String -> Either (Error a) a
parseString p s = case(readP_to_S p s) of 
					[(r,"")] -> Right r
					[] -> Left $ NoParses
					[(r,rem)] -> Left $ Remainder (r,rem)
					parses -> Left $ Ambiguous parses