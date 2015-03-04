data Tree a = Leaf a | Node (Tree a) (Tree a)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

type State = Int

fresh :: ST Int
fresh = S (\n -> (n,n+1))

mlabel  :: Tree a -> ST (Tree (a,Int))
mlabel (Leaf x) = do n <- fresh
					return (Leaf (x,n))
mlabel (Node l r) = do l' <- mlabel l
					r' <- mlabel r
					return (Node l' r')
					
label :: Tree a -> Tree (a,Int)
label t = fst (apply (mlabel t) 0)

-- the IO monad
return :: a -> IO a
(>>=)  :: IO a -> (a -> IO b) -> IO b
getChar :: IO Char
putChar :: Char -> IO ()

getLine :: IO String
getLine = do x <- getChar
			if x == '\n' then
				return []
			else
				do xs <- getLine
					return (x : xs)
					
type World = ...
type IO a = World -> (a,World)

--derived primitives
--map
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do x <- mx
				return (f x)

--concat
join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
			  x <- mx
			  reutrn xs
			  
(>>)  :: Monad m => m a -> m b -> m b
mx >> my = do _ <- mx
			 y <- my
			 return y
			 
			 
sequence :: Monad m => [m a] -> m [a]
sequence []  = return []
sequence (mx:mxs) = do x <- mx
					   xs <- sequence mxs
					   return (x:xs)
				
					
