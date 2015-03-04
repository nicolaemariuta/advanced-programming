-- Assignment1 MSM, Radu Ionita, Nicolae Mariuta

module MSM where

-- we want to use monads here
import Control.Monad
import Control.Applicative
import Data.List as List

-- and you might also find Maps useful
import qualified Data.Map as Map

-- | The type of instructions for the MSM.
data Inst 
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
    deriving (Eq,Show)
 
-- | The type `Prog` represents programs for the MSM.
type Prog = [Inst]

-- | The type `Stack` represents the stack of the MSM.
type Stack = [Int]

-- | Regs is the type for keeping track of registers
type Regs = Map.Map Int Int

-- | This data type encapsulates the state of a running MSM.
data State = State
             { prog  :: Prog
             , pc    :: Int
             , stack :: Stack
             , regs  :: Regs
             }
           deriving (Show, Eq)

data ErrorType = StackUnderflow
               | UnallocatedRegister Int
               | RegisterAlreadyAllocated
               | InvalidPC
               | Unspec String
               deriving (Show, Read, Eq)		   
data Error = Error { errorType :: ErrorType, 
					 state :: State }   
           deriving (Show, Eq)

-- | `initial` constructs the initial state of an MSM running the
-- given program.
initial :: Prog -> State		
initial p = State { prog= p
				  , pc = 0
				  , stack = []
				  , regs = Map.fromList([])
				  }

--createMSM :: MSM a -> State -> (a,State)
--createMSM (MSM f)a = f a

-- | This is the monad that is used to implement the MSM. 
newtype MSM a = MSM (State -> Either String (a,State))  

instance Monad MSM where
-- (>>=) :: MSM a -> (a -> MSM b) -> MSM b       

  -- we got an important understanding (though not fully) of the Monad
  -- in the exercises section. we learned by example. The Abstract concept
  -- of the Monad is still rusty for us
	(MSM p) >>= f = MSM(\x -> case p x of
							Right m -> let Right(a,x') = p x;
								           (MSM q) = f a
							           in  q x'
							Left m -> Left m
						)
	-- return :: a -> MSM a
	return a = MSM(\x -> Right(a,x))

-- at the exercises	section we were told that we should not focus on the
-- fail function of the Monad

{-	fail :: String -> MSM a
	fail s = MSM (\x -> Left s)
-}
-- Remember to make your monad a functor as well
-- the function was a greate help from the teacher's slides
instance Functor MSM where
	fmap f xs = xs >>= return . f

-- the function was a greate help from the teacher's slides
-- And perhaps also an Applicative
instance Applicative MSM where
	pure = return
	df <*> dx = df >>= \f -> dx >>= return . f

-- | `get` returns the current state of the running MSM.
get :: MSM State
get = MSM $ \s -> Right(s,s)

-- | set a new state for the running MSM.
set :: State -> MSM ()
set m = MSM $ \s -> Right((), m)

-- | modify the state for the running MSM according to
-- the provided function argument
modify :: (State -> State) -> MSM ()
modify f = MSM (\s -> Right ((), f s))

-- | This function provides the instruction the PC currently points
-- to. If the PC is out of bounds, the MSM halts with an error.
getInst :: MSM Inst
getInst = do
  stat <- get
  if pc stat > length (prog stat) || (pc stat) < 0 
     then fail "Program Counter is not valid " 
     else return $ (prog stat) !! (pc stat)

-- | This function runs the MSM.
interp :: MSM ()
interp = run
    where run = do inst <- getInst
                   cont <- interpInst inst
                   when cont run

-- | This function interprets the given instruction. It returns True
-- if the MSM is supposed to continue it's execution after this instruction
interpInst :: Inst -> MSM Bool
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
      newreg a thisState
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

-- Push Function 		   		   	   
push :: Int -> State -> MSM Bool  
push a x = do
  set x{stack = a:stack x, pc = pc x+1}
  return True

--Pop Function
pop :: State -> MSM Bool
pop x = do 
  let (first:other) = stack x
  let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
            | otherwise = set x{stack = other, pc = pc x+1}  
  verif
  return True

--Dup Function
dup :: State -> MSM Bool
dup x = do
  let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
            | otherwise = set x{stack = head(stack x):stack x, pc = pc x+1}
  verif
  return True 
	
swapStack::Stack -> Stack
swapStack xStack = 
  let (first, second) = (head xStack, head $ tail xStack) in
  second : first : drop 2 xStack

--Swap Function
swap :: State -> MSM Bool
swap x = do
  if length  (stack x) < 2 
    then fail $ showError Error{errorType = StackUnderflow}
    else set x{stack = swapStack (stack x), pc = pc x + 1} 
  return True

-- NewReg Function
newreg :: Int -> State -> MSM Bool
newreg aReg x = do
  if Map.member aReg (regs x) 
    then fail $ showError Error{errorType = RegisterAlreadyAllocated}
    else set x{regs = Map.insert aReg 0 (regs x), pc = pc x + 1 }
  return True

-- Load Function
load :: State -> MSM Bool
load x = do
  let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
            | otherwise = set x{stack = regs x Map.! head (stack x):tail (stack x), pc = pc x+1}
  verif
  return True

-- Store Function
store :: State -> MSM Bool
store x = do
  let verif | length (stack x) < 2 = fail $ showError Error{errorType = StackUnderflow}   
            | otherwise = set x{regs = Map.insert (stack x !! 1) (head (stack x)) (regs x), 
                          stack = drop 2 (stack x), pc = pc x+1}
  verif
  return True

neg :: State -> MSM Bool
neg x = do
  let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
            | otherwise = set x{stack = head(stack x)*(-1) : tail(stack x), pc = pc x+1} 
  verif      
  return True

-- Add Function
add :: State -> MSM Bool
add x = do
  let verif | length (stack x) < 2 = fail $ showError Error{errorType = StackUnderflow}
            | otherwise = set x{stack = head(stack x) + head(tail(stack x)) : drop 2 (stack x), pc = pc x+1} 
  verif
  return True

-- Jump Function
jmp :: State -> MSM Bool
jmp x = do
  let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
            | otherwise = set x{pc = head(stack x), stack = tail(stack x) }  
  verif
  return True

cjmp :: Int -> State -> MSM Bool
cjmp a x = do
  let verif | List.null (stack x) = fail $ showError Error{errorType = StackUnderflow}
            | head (stack x) < 0 = set x{stack = tail (stack x), pc = a}
            | otherwise = set x{stack = tail (stack x), pc = pc x + 1}
  verif
  return True

-- it s rather strange, if i uncomment the last 3 patterns, haskell throws me a
--  Parse error on input error on the last 3 patterns. We could not find out what 
-- the problem is
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
	   


p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]
