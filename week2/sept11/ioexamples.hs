{-
  Example code produced for Advanced Programming lecture.

  Working with IO

  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

readAdd :: Num a => IO a -> IO a -> IO a
readAdd x y =
  do m <- x
     n <- y
     return (m+n)

res = readAdd getInt getInt2

getInt :: IO Int
getInt = do
  s <- getLine
  return $ read s

getInt2 :: IO Int
getInt2 = do
  putStr "Gimme int>"
  s <- getLine
  putStrLn "Thx"
  return(read s)

listOfIO = [getInt2, getInt, res]


doEmAll :: [IO a] -> IO [a]
doEmAll [] = return []
doEmAll (ioa : ioas) = do
  v <- ioa
  vs <- doEmAll ioas
  return $ v : vs

-- 




















-- doEmAll :: Monad m => [m a] -> m[a]
-- doEmAll [] = return []
-- doEmAll (x:xs) = do
--   s  <- x
--   ss <- doEmAll xs
--   return $ s:ss
