{-
Example code produced "during" Advanced Programming lecture.

Introducing the Maybe monad step by step
-}

(>>==) :: Maybe a -> (a -> Maybe b) -> Maybe b
ret :: a -> Maybe a

ret x = Just x

m >>== f =
  case m of
    Just a  ->  f a
    Nothing -> Nothing

checkEven n =
  if n `mod` 2 == 0 then Just n
  else Nothing

checkedAdd x y =
  case x of
    Just m ->
      case y of
        Just n  -> Just (m+n)
        Nothing -> Nothing
    Nothing -> Nothing

checkedAdd2 x y =
  x  >>==   \m ->
  y  >>==   \n ->
  ret (m+n)

-- -- one version using plain-old functions
-- checkedAdd2 x y =
--   x >>== \ m ->
--   y >>== \ n ->
--   ret$ m+n

-- one version using the real >>= and return
checkedAdd2' x y =
  x >>= \ m ->
  y >>= \ n ->
  return $ m+n


checkedAdd3 x y = do
  m <- x
  n <- y
  return $ m+n



