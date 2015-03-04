inc ::[Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

inc = map (+1)
sqr = map (^2)

--a simple evaluator
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x) = eval x `div` eval y

data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv n m = if m ==0 then Nothing else Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
				Nothing -> Nothing
				Just n -> case eval y of
							Nothing -> Nothing
							Just m -> safediv n map
							
seqn  :: Maybe a -> Maybe b -> Maybe (a,b)
seqn Nothing _ = Nothing
seqn - Nothing = Nothing
seqn (Just x) (Just y) = Just (x,y)

eval (Val n) = Just n
eval (Div x y) = apply f (eval x `seqn` eval y)
				 where f(n,m) = safediv n map

apply :: (a-> Maybe b) -> Maybe a -> Maybe b
apply f Nothing = Nothing
apply f (Just x) = f x

eval (Op x y z) = apply f (eval x `seqn (eval y `seqn` eval z))
				where f (a,(b,c)) = ...
				
--Combining sequencing and processing
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
m >>= f = case m of
			Nothing -> Nothing
			Just x -> f x
			
(>>=) :: Maybe a -> (a-> Maybe b) -> Maybe b
Nothing >>=  _ = Nothing
(Just x) >>= f = f x

eval (Val n) = Just n
eval (Div x y) = eval x >>= (\n ->
				eval y >>= (\m ->
				safediv n m))
				
eval (Val n) = Just n
eval (Div x y) = do n <- eval x
					m <- eval y
					safediv n map
					
--Monads in Haskell
return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b

class Eq a where
	(==) :: a -> a -> Bool
	(/=) :: a -> a -> Bool
	
	x/= y = not (x==y)
	
instance Eq Bool where
	False == False = True
	True == True = True
	_    ==_     = False
	
class Monad m where
	return :: a -> m a
	(>>=) :: m a -> (a -> m b) -> m b
	
instance Monad Maybe where
	-- return :: a -> Maybe a
	return x  = Just x
	
	--(>>=)   :: Maybe a -> (a -> Maybe b) -> Maybe b
	Nothing >>= = Nothing
	(Just x) >>= f = f x
	
instance Monad [] where
	-- return :: a -> [a]
	return x = [x]
	
	--(>>=) :: [a] -> (a -> [b]) ->[b]
	xs >>= f = concat (map f xs)
	
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
				 y <- ys
				 return (x,y)
				 
pairs xs ys = [(x,y) | x <- xs, y <- ys]

type State = ...
type ST = State -> State
type ST a = State -> (a, State)

instance Monad ST where
	-- return a -> ST a
	return x = \s -> (x,s)
	
	-- (>>=) :: ST a -> (a->ST b) -> ST b
	st >>= f = \s -> let (x,s') = st s in f x s'
	
data ST a = S(State -> (a,State))

apply :: ST a -> State -> (a,State)
apply (S f) x = f x/

instance Monad ST where
	--return :: a -> ST a
	return x = S (\s -> (x,s))
	
	-- (>>=) :: ST a -> (a -> ST b) -> ST b
	st >>= f = S (\s -> let (x,s') = apply st s in apply (f x) s')
	



	
	
	

	




