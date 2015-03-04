import Data.Char
import Text.ParserCombinators.ReadP

type Parser a = ReadP a

digit :: Parser Char
digit = satisfy (\c -> c `elem` [0..9])
				(`elem` [0..9])
				
digits :: Parser Int
digits = do ns <- many1 digt
			let n = read ns 
			return n
			
do symbol "[" 
   commasep
   symbol "]"
   
where 
	commasep :: Parser [Int]

--L := "[" E "]"
--E := F | e
--F := nFopt
--Fopt := "," F | e

