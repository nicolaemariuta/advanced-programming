module MSM where

import Control.Monad
import Control.Applicative
import Data.List as List

import qualified Data.Map as Map


data  Inst
	= PUSH Int
	| POP
	| DUP
	| SWAP
	| NEWREG Int
	| LOAD
	| STORE
	| NEG
	| ADD
	| JMP
	| CJMP Int
	| HALT
	deriving (Eq, Show)
	
type Porg = [Inst]
type Stack = [Int]
type Regs = Map.Map Int Int

data State = State
			{ prog :: Prog
			, pc :: Int
			, stack :: Stack
			, regs :: Regs
			}
		deriving (Eq, Show)

data ErrorType = StackUnderflow
				| UnallocatedRegister Int
				| RegisterAlreadyAllocated
				| InvalidPC
				| Unspec String
			deriving (Show, Read, Eq)
			
data Error = Error { errorType :: ErrorType
				   , state :: State }
				deriving (Show, Eq)
				
				
initial :: Prog -> State
initial p = State { Prog = p,
					pc = 0,
					stack = [],
					regs = Map.fromList([])}
					
newtype MSM a = MSM (State -> Either String (a, State))

instance Monad MSM where
	(MSM p) >>= f   = MSM(\x -> case p x of 
								Right m -> let Right(a,x') = p x;
											   (MSM q) = f a
										   in q x'
								Left m -> Left m
								
	return a = MSM(\x -> Right(a,x))
	
	
instance Functor MSM where
	fmap f xs = xs >>= return . f
	
instance Applicative MSM where
	pure = return
	df <*> dx = df >>= \f -> dx >>= return . f
	
	
get :: MSM State
get = MSM $ \s -> Right(s,s)

set :: State -> MSM ()
set m = MSM $ \s -> Right((), m)

modify :: (State -> State) -> MSM ()
modify f = MSM(\s -> Right ((), f s))

getInst :: MSM Inst
getInst = do 
	stat <- get
	if pc stat > length (prog stat) || (pc stat) < 0
		then fail "Program counter is not valid"
		else return $ (prog stat) !! (pc stat)
		
interp :: MSM ()
interp = run
	where run = do inst <- getInst
				   cont <- interpInst inst
				   when cont run
	
											   
interpInst  :: Inst -> MSM Bool
interpInst instruct = do
	thisState <- get
	case instruct of 
		PUSH a -> do
			push a thisState
		POP -> do
			pop thisState
		DUP -> do
			dup thisState
		SWAP -> do
			swap thisState
		NEWREG a -> do
			newReg a thisState
		LOAD -> do
			load thisState
		STORE -> do
			store thisState
		NEG -> do
			neg thisState
		ADD -> do
			add thisState
		JMP -> do
			jmp thisState
		CJMP a -> do
			cjmp a thisState
		HALT -> do
			return False
		
		
push :: Int -> State -> MSM Bool
push a x = do
	set x{stack = a:stack x, pc = pc x+1}
	return True
	
pop :: State -> MSM Bool
pop x = do
	let (first:other) = stack xs
	let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
			  | otherwise = set x{stack = other, pc = pc x+1}
	verif
	return True

swapStack :: Stack -> Stack
swapStack xStack = 
	let (first,second) = (head xStack, head $ tail xStack) in
	second : first : drop 2 xStack


swap: Stack -> MSM Bool
swap x = do
	if length (stack x) < 2
		then fail $ showError Error{errorType = StackUnderflow}
		else set x{ stack = swapStack (stack x), pc = pc x+1}
	return True

newReg :: Int -> State -> MSM Bool
newReg aReg x = do
		if Map.member aReg (regs x)
			then fail $ showError Error{errorType = RegisterAlreadyAllocated}
			else set x{regs = Map.insert aReg 0 (regs x), pc = pc x +1 }
		return True 

load :; State -> MSM Bool
load x = do
	let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
			  | otherwise = set x{stack = reg x Map.! head (stack x):tail (stack x), pc = pc x+1}
	verif
	return True
	
store :: State -> MSM Bool
store x = do
	let verif | length (stack x) <2 = fail $ showError Error{errorType = StackUnderflow}
			  | otherwise = set x{regs = Map.insert (stack x !! 1) (head (stack x)) (regs x),
									stack = drop 2 (stack x), pc = pc x+1} 
	
neg :: State -> MSM Bool
neg x = do
	let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
			  | otherwise = set x{stack = head(stack x)*(-1) : tail(stack x), pc = pc x+1}
	verif 
	return True
	
add :: State -> MSM Bool
add x = do 
	let verif | length (stack) < 2 = fail $ showError Error{errorType = StackUnderflow}
			  | otherwise = set x{stack = head(stack x) + head(tail(stack x)) : drop 2 (stack x), pc = pc x+1}
	verif
	return True
	
jmp :: State -> MSM Bool
jmp x = do
	let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
			  | otherwise = set x{pc = head(stack x), stack = tail(stack x) }
			  
	verif
	return True
	
cjmp :: Int -> State -> MSM Bool
cjmp a x = do
	let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
			  | head (stack x) < 0 = set {stack = tail (stack x), pc = a}
			  | otherwise = set x{stack = tail (stack x), pc = pc x + 1}
	verif
	return True


showError :: Error -> String
showError x = case (errorType x) of
	InvalidPC -> "Program Counter is not valid"
	StackUnderflow -> "StackUnderflow"
	RegisterAlreadyAllocated -> "Register is already registered"
	UnallocatedRegister a -> "Register is unalocated " ++ show a
	Unspec a -> "Unspec String" ++ show a

   -- | Run the given program on the MSM
runMSM :: Prog -> Either String State
runMSM p = let (MSM f) = interp
		   in fmap snd $ f $ initial p 
	   

	
  

























	
	
	
	
	