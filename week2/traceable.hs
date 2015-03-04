newtype Trace a = T (a, String)

instance Monad Trace where
    -- (>>=) :: Trace a -> (a -> Trace b) -> Trace b
    (T p) >>= f = 

    -- return :: a -> Trace a
    return x = 