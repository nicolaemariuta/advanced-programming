{-# OPTIONS_GHC -Wall #-}
module AntaInterpreter
       ( runProg
       , Error (..)
       )
       where

import AntaAST

-- You might need the following imports
import Control.Applicative
import Control.Monad
import qualified Data.Map as Map


-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data ErrorType = DivideByZero
               | VariableNotFound
               | WrongInput
               | InvalidArguments
               | InvalidExpression
               | NotSameArguments
               | NotImplementedYet
               | Unspec String
               deriving (Show, Read, Eq)		   
data Error = Error { errorType :: ErrorType, 
					 state :: AntaState}   
           deriving (Show, Eq)

-- | A mapping from names to values
type Env = Map.Map Name Value
--state of AntaM program
data AntaState = AntaState
             { prog  :: Program
             , env  :: Env
			 , res :: Result
			 }
           deriving (Show, Eq)
           
 -- | `initial` constructs the initial state of an Antaresa program
-- given program.
initial :: Program -> AntaState		
initial p = AntaState { prog= p
                      , env = Map.fromList([])
                      , res = []
                        }
  
 -- | The basic monad in which execution of a Antaresia program takes
 --place.  Maintains the global state and whether or not an error has
 --occurred.
newtype AntaM a = AntaM ( AntaState -> Either String (a,AntaState))

instance Functor AntaM where
  fmap = liftM

instance Applicative AntaM where
  pure = return
  (<*>) = ap

instance Monad AntaM where
    (AntaM p) >>= f = AntaM(\x -> case p x of
							Right m -> let Right(a,x') = p x;
								           (AntaM q) = f a
							           in  q x'
							Left m -> Left m
						)
	-- return :: a -> AntaM a
    return a = AntaM(\x -> Right(a,x))


--change state of AntaM
modify :: (AntaState -> AntaState) -> AntaM ()
modify f = AntaM (\s -> Right ((), f s))  

 -- | `get` returns the current state of the running AntaM.
getAntaState :: AntaM AntaState
getAntaState = AntaM $ \s -> Right(s,s)

-- | set a new state for the running AntaM.
setAntaState :: AntaState -> AntaM ()
setAntaState m = AntaM $ \s -> Right ((), m)

--change Env in in program 
updateEnv :: Name -> Value -> AntaM ()
updateEnv name val = 
                    do
                        s <- getAntaState
                        setAntaState  s{ env = Map.insert name val (env s)}
                        
--function that returns the next  Decl in program  
--fail the program if there is no Decl left                     
getDecl :: AntaM Decl
getDecl = do
  stat <- getAntaState
  if length (prog stat) == 0
     then fail "Error no expression left"
     else return $ (prog stat) !! 0
     
 


-- | This function runs the AntaM
interp :: AntaM()
interp = run
    where run = do (n,e) <- getDecl
                   cont <- evalExpr n e
                   when cont run 
                    
  
--function to calculate range expression
range :: Integer -> Integer -> Integer -> [Integer]
range start stop step = [start,(start+step)..stop]
      --  return [start,b..stop]
            
-- | This function interprets the given expression. It returns True
-- if AntaM is supposed to continue it's execution after this instruction
evalExpr :: Name -> Expr -> AntaM Bool
evalExpr n expr = do
    thisState <- getAntaState
    case expr of
        IntConst a -> do
           intConst n a thisState
        TrueConst -> do
           trueConst n thisState
        FalseConst -> do
           falseConst n thisState
        Name a -> do
            nameExp n a thisState
        Range a -> do
            rangeExpr n a thisState
        Plus e1 e2 -> do
            plusExpr n e1 e2 thisState
        Minus e1 e2 -> do
            minusExpr n e1 e2 thisState
        Mult e1 e2 -> do
            multExpr n e1 e2 thisState
        Div e1 e2 -> do
            divExpr n e1 e2 thisState
        Modulus e1 e2 -> do
            modulusExpr n e1 e2 thisState
        Equal e1 e2 -> do
            equalExpr n e1 e2 thisState
        ListComp l -> do
            listComp n l thisState
        _ ->fail $ showError Error{errorType = InvalidExpression}  
        
    
--function that inserts into AntaState the value and name of new IntConst
--at the end checks if there is any Decl left into AntaM
intConst :: Name -> Integer -> AntaState -> AntaM Bool
intConst n a x = do 
    setAntaState x{prog = tail (prog x), env = Map.insert n (IntVal a) (env x), res = (n,IntVal a):(res x)}
    newState <- getAntaState
    if length (prog newState) == 0
        then return False
        else return True
        
--function that inserts into AntaState the value and name of new TrueConst
--at the end checks if there is any Decl left into AntaM        
trueConst :: Name -> AntaState -> AntaM Bool
trueConst n x = do 
    setAntaState x{prog = tail (prog x), env = Map.insert n (TrueVal) (env x), res = (n,TrueVal):(res x)}
    newState <- getAntaState
    if length (prog newState) == 0
        then return False
        else return True        
 
--function that inserts into AntaState the value and name of new FalseConst
--at the end checks if there is any Decl left into AntaM 
falseConst :: Name -> AntaState -> AntaM Bool
falseConst n x = do 
    setAntaState x{prog = tail (prog x), env = Map.insert n (FalseVal) (env x), res = (n,FalseVal):(res x)}
    newState <- getAntaState
    if length (prog newState) == 0
        then return False
        else return True  

--function that attributes to variable n the value of variable a
--at the end checks if there is any Decl left into AntaM
--returns error if varibale n is not in the program
nameExp :: Name -> Name -> AntaState ->  AntaM Bool     
nameExp n a x = do
    case Map.lookup a (env x) of 
                    Just v -> setAntaState x{prog = tail (prog x), env = Map.insert n v (env x), res = (n,v):(res x)}
                    Nothing -> fail $ showError Error{errorType = VariableNotFound}                     
    newState <- getAntaState
    if length (prog newState) == 0
        then return False
        else return True 
 
--function to evaluate the range expressions according to the number of arguments
--at the end checks if there is any Decl left into AntaM 
rangeExpr :: Name -> Args3 -> AntaState -> AntaM Bool
rangeExpr n a x = do 
    case a of 
        A1 (IntConst e1 ) -> setAntaState x{prog = tail (prog x), env = Map.insert n ( List $ fmap IntVal $ range 0 e1 1) (env x), res = (n,(List $ fmap IntVal $ range 0 e1 1 )):(res x)}
        A2 (IntConst e1 ) (IntConst e2 ) -> setAntaState x{prog = tail (prog x), env = Map.insert n  (List $ fmap IntVal $ range e1 e2 1) (env x), res = (n, (List $ fmap IntVal $ range e1 e2 1 )):(res x)}
        A3 (IntConst e1 ) (IntConst e2 ) (IntConst e3 )->  setAntaState x{prog = tail (prog x), env = Map.insert n (List $ fmap IntVal $ range e1 e2 e3) (env x), res = (n, List $ fmap IntVal $ range e1 e2 e3 ):(res x)}
        _ -> fail $ showError Error{errorType = WrongInput}               
    newState <- getAntaState
    if length (prog newState) == 0
        then return False
        else return True  

--function to evaluate the Plus operation expressions 
--at the end checks if there is any Decl left into AntaM
--returns error in case any of the arguments is not integer        
plusExpr :: Name -> Expr -> Expr -> AntaState -> AntaM Bool
plusExpr n e1 e2 x = do
       case e1 of
          IntConst v1 ->   do
                            case e2 of 
                                IntConst v2 -> setAntaState x{prog = tail (prog x), env = Map.insert n (IntVal (v1+v2)) (env x), res = (n,IntVal (v1+v2)):(res x)}
                                _ -> fail $ showError Error{errorType = InvalidArguments} 
          _ -> fail $ showError Error{errorType = InvalidArguments} 
       newState <- getAntaState
       if length (prog newState) == 0
       then return False
       else return True     
            

--function to evaluate the Minus operation expressions 
--at the end checks if there is any Decl left into AntaM
--returns error in case any of the arguments is not integer            
minusExpr :: Name -> Expr -> Expr -> AntaState -> AntaM Bool
minusExpr n e1 e2 x = do
       case e1 of
          IntConst v1 ->   do
                            case e2 of 
                                IntConst v2 -> setAntaState x{prog = tail (prog x), env = Map.insert n (IntVal (v1-v2)) (env x), res = (n,IntVal (v1-v2)):(res x)}
                                _ -> fail $ showError Error{errorType = InvalidArguments} 
          _ -> fail $ showError Error{errorType = InvalidArguments} 
       newState <- getAntaState
       if length (prog newState) == 0
       then return False
       else return True              
 

--function to evaluate the Multiplication operation expressions 
--at the end checks if there is any Decl left into AntaM
--returns error in case any of the arguments is not integer 
multExpr :: Name -> Expr -> Expr -> AntaState -> AntaM Bool
multExpr n e1 e2 x = do
       case e1 of
          IntConst v1 ->   do
                            case e2 of 
                                IntConst v2 -> setAntaState x{prog = tail (prog x), env = Map.insert n (IntVal (v1*v2)) (env x), res = (n,IntVal (v1*v2)):(res x)}
                                _ -> fail $ showError Error{errorType = InvalidArguments} 
          _ -> fail $ showError Error{errorType = InvalidArguments} 
       newState <- getAntaState
       if length (prog newState) == 0
       then return False
       else return True   
 
--function to evaluate the Divide operation expressions 
--at the end checks if there is any Decl left into AntaM
--returns error in case any of the arguments is not integer or the second operator is 0
divExpr :: Name -> Expr -> Expr -> AntaState -> AntaM Bool
divExpr n e1 e2 x = do
       case e1 of
          IntConst v1 ->   do
                            case e2 of 
                                IntConst v2 -> 
                                      if v2 /= 0
                                      then setAntaState x{prog = tail (prog x), env = Map.insert n (IntVal (v1 `div` v2)) (env x), res = (n,IntVal (v1 `div` v2)):(res x)}
                                      else fail $ showError Error{errorType = DivideByZero} 
                                _ -> fail $ showError Error{errorType = InvalidArguments} 
          _ -> fail $ showError Error{errorType = InvalidArguments} 
       newState <- getAntaState
       if length (prog newState) == 0
       then return False
       else return True        


--function to evaluate the Modulus operation expressions 
--at the end checks if there is any Decl left into AntaM
--returns error in case any of the arguments is not integer or the second operator is not >0      
modulusExpr :: Name -> Expr -> Expr -> AntaState -> AntaM Bool
modulusExpr n e1 e2 x = do
       case e1 of
          IntConst v1 ->   do
                            case e2 of 
                                IntConst v2 -> 
                                      if v2 >0 
                                      then setAntaState x{prog = tail (prog x), env = Map.insert n (IntVal (v1 `mod` v2)) (env x), res = (n,IntVal (v1 `mod` v2)):(res x)}
                                      else fail $ showError Error{errorType = DivideByZero} 
                                _ -> fail $ showError Error{errorType = InvalidArguments} 
          _ -> fail $ showError Error{errorType = InvalidArguments} 
       newState <- getAntaState
       if length (prog newState) == 0
       then return False
       else return True       
       


--function to evaluate the Equals operation expressions 
--at the end checks if there is any Decl left into AntaM
--returns error in case arguments are not of the same type      
equalExpr :: Name -> Expr -> Expr -> AntaState -> AntaM Bool
equalExpr n (IntConst e1) (IntConst e2) x = do
       if e1 == e2
        then setAntaState x{prog = tail (prog x), env = Map.insert n (TrueVal) (env x), res = (n,TrueVal):(res x)}
        else setAntaState x{prog = tail (prog x), env = Map.insert n (FalseVal) (env x), res = (n,FalseVal):(res x)}
       newState <- getAntaState
       if length (prog newState) == 0
        then return False
        else return True       
equalExpr n (TrueConst) (TrueConst) x = do
       setAntaState x{prog = tail (prog x), env = Map.insert n (TrueVal) (env x), res = (n,TrueVal):(res x)}
       newState <- getAntaState
       if length (prog newState) == 0
        then return False
        else return True 
equalExpr n (FalseConst) (FalseConst) x = do
       setAntaState x{prog = tail (prog x), env = Map.insert n (TrueVal) (env x), res = (n,TrueVal):(res x)}
       newState <- getAntaState
       if length (prog newState) == 0
        then return False
        else return True 
equalExpr n (FalseConst) (TrueConst) x = do
       setAntaState x{prog = tail (prog x), env = Map.insert n (FalseVal) (env x), res = (n,FalseVal):(res x)}
       newState <- getAntaState
       if length (prog newState) == 0
        then return False
        else return True         
equalExpr n (TrueConst) (FalseConst) x = do
       setAntaState x{prog = tail (prog x), env = Map.insert n (FalseVal) (env x), res = (n,FalseVal):(res x)}
       newState <- getAntaState
       if length (prog newState) == 0
        then return False
        else return True  
equalExpr n (ListComp e1) (ListComp e2) x = do
       if e1 == e2
        then setAntaState x{prog = tail (prog x), env = Map.insert n (TrueVal) (env x), res = (n,TrueVal):(res x)}
        else setAntaState x{prog = tail (prog x), env = Map.insert n (FalseVal) (env x), res = (n,FalseVal):(res x)}
       newState <- getAntaState
       if length (prog newState) == 0
        then return False
        else return True            
equalExpr _ _ _ _ = fail $ showError Error{errorType = NotSameArguments} 

--function to evaluate the ListCom operation expressions 
--returns error because I did not manage to implement these
listComp :: Name -> ListComp -> AntaState -> AntaM Bool 
listComp _ _ _ = fail $ showError Error{errorType = NotImplementedYet}         


--show corresponding message error in case of failure
showError :: Error -> String
showError x = case (errorType x) of
    DivideByZero -> "Cannot divide by zero"
    VariableNotFound -> "Variable was not found"
    WrongInput -> "Value give is of wrong type"
    InvalidArguments -> "Operator arguments must be integers"
    InvalidExpression -> "Expression is invalid"
    NotSameArguments -> "Arguments of equals must be same type"
    NotImplementedYet -> "List comprehesions not implemented yet! They are about to come in next patch."
    Unspec a -> "Unspec String" ++ show a
    
-- | Run the given program on the AntaM
program :: Program -> Either String AntaState
program p =let (AntaM f) = interp
		   in fmap snd $ f $ initial p 
 
           
           
--starts the program function and checks the result to extract error or final Result            
runProg :: Program -> Either String Result
runProg p = case program p of
    Left e -> Left e
    Right n -> Right (Map.toList(env n))
   









