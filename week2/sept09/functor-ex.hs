{-
Example code produced "during" Advanced Programming lecture.

Making some type constructors instances of Functor
-}



-- import Prelude hiding (map)   -- left here if you should ever need it.

data KenList a = Nil
               | Cons a (KenList a)
               deriving (Eq, Show)

instance Functor KenList where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs) 


data Map k v = Empty
             | Node k v (Map k v) (Map k v)
             deriving (Eq, Show)

fromSorted xs = build [ (Empty, x) | x <- xs]
  where
    build [] = Empty
    build [(Empty, (k,v))] = Node k v Empty Empty
    build([(Node k v t1 t2, x)]) = Node k v t1 $ build [(t2, x)]
    build xs = build $ sweep $ xs

    sweep ((t1,(k,v)) : (t2,x) : ts) = (Node k v t1 t2, x) : sweep ts
    sweep ts = ts
 
    
m1 = fromSorted $ [ (x,x) | x <- [1..6]]

instance Functor (Map k) where
  --  fmap :: (a -> b) -> Map k a -> Map k b
  fmap _ Empty = Empty
  fmap f (Node k x left right) = 
     Node k (f x) (fmap f left) (fmap f right)
