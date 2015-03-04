module MSM where

-- we want to use monads here
import Control.Monad
import Control.Applicative

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

--data Stack a = Bottom | Item a (Stack a)
--pop :: Stack a -> (a, Stack a)
--pop (Item a as) = (a,as)
--pop (Bottom) = error "Stack overflow"

--push :: a -> Stack a -> Stack a
--push = Item

--push :: a -> Stack a -> ((),Stack a)
--push a as = ((),Item a as)


-- add::MSM()      -->no value
-- add = do 
--        x <- pop
--        y <- pop
--        push (x+y)
--
-- add s = let
--           (x, s1) = pop s
--           (y, s2) = pop s1
--         in push s2 (x+y)





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
					 state :: State }    -- ??
           deriving (Show, Eq)


-- | `initial` constructs the initial state of an MSM running the
-- given program.
initial :: Prog -> State		
initial p = State{prog= p, pc = 0, stack = [], regs = Map.fromList([])}

-- 
--createMSM :: MSM a -> State -> (a,State)
--createMSM (MSM f)a = f a

-- | This is the monad that is used to implement the MSM. 
newtype MSM a = MSM (State -> (a,State))  -- ??
--newtype MSM a = MSM (Stack Int -> Either ErrorType (a,StackInt))  -- ??
														--f a ::SM b
instance Monad MSM where
	-- (>>=) :: MSM a -> (a -> MSM b) -> MSM b         p::State ->(a,State)
	(MSM p) >>= f = MSM(\x -> let
								(a,x') =  p x
								(MSM q)= f a
							  in  q x')								

	-- return :: a -> MSM a
	return a = MSM(\x ->  (a,x))

-- Remember to make your monad a functor as well
instance Functor MSM where
  -- fmap :: (a->b) -> MSM a -> MSM b
	fmap f m = m >>= return . f

-- And perhaps also an Applicative
instance Applicative MSM where
 pure = return
 df <*> dx = df >>= \f -> dx >>= return . f
 
-- | `get` returns the current state of the running MSM.
get :: MSM State
get = MSM $ \s -> (_, s)

-- | set a new state for the running MSM.
set :: State -> MSM ()
set m = MSM $ \s -> ((), m)

-- | modify the state for the running MSM according to
-- the provided function argument
modify :: (State -> State) -> MSM ()
modify f = undefined


-- | This function provides the instruction the PC currently points
-- to. If the PC is out of bounds, the MSM halts with an error.
--getInst :: unde
getInst = undefined


-- | This function runs the MSM.
interp :: MSM ()
interp = run
    where run = do inst <- getInst
                   cont <- interpInst inst
                   when cont run

-- | This function interprets the given instruction. It returns True
-- if the MSM is supposed to continue it's execution after this
-- instruction.
interpInst :: Inst -> MSM Bool
interpInst inst = undefined

{- -- | Run the given program on the MSM
runMSM :: Prog -> ...
runMSM p = let (MSM f) = interp 
           in fmap snd $ f $ initial p  -}
		   
--functions
--pop :: MSM Int
--pop = do		   
 		   		   
		   
		   
		   
	{-	   
-- Function POP
pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs) 

-- Function PUSH
push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

-}



-- example program, when it terminates it leaves 42 on the top of the stack
p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]

-- testare ghci
--       :t add  =====> SM(Stack Int -> ((),StackInt)
--       
--       let
--       	(SM f) =add
--       in f (Item5 (Item10 Bottom))


