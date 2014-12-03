putStr' :: String -> IO ()

putStr' []     = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()

putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

--putStrLn' [] = putChar '\n'
--putStrLn' xs = putStr' xs >> putChar '\n'

--putStrLn' [] = putChar '\n'
--putStrLn' xs = putStr' xs >>= \ x -> putChar '\n'

--putStrLn' [] = putChar '\n'
--putStrLn' xs = putStr' xs >> putStr' "\n"

--putStrLn' [] = return ""
--putStrLn' xs = putStrLn' xs >> putStr' "\n"

--putStrLn' [] = putChar "\n"
--putStrLn' xs = putStr' xs >> putChar '\n'

getLine' :: IO String

getLine' = get []

get :: String -> IO String

get xs
  = do x <- getChar
       case x of
            '\n' -> return xs
            _ -> get (xs ++ [x])

interact' :: (String -> String) -> IO ()

interact' f
	= do input <- getLine'
	     putStrLn' (f input)

sequence_' :: Monad m => [m a] -> m ()

--sequence_' [] = return []
--sequence_' (m : ms) = m >> \ _ -> sequence_' ms

--sequence_' [] = return ()
--sequence_' (m : ms) = (foldl (>>) m ms) >> return ()

--sequence_' ms = foldl (>>) (return ()) ms

sequence_' [] = return ()
sequence_' (m:ms) = m >> sequence_' ms

--sequence_' [] = return ()
--sequence_' (m:ms) = m >>= \ _ -> sequence_' ms

--sequence_' ms = foldr (>>=) (return ()) ms
--sequence_' ms = foldr (>>) (return ()) ms

--sequence_' ms = foldr (>>=) (return []) ms

sequence' :: Monad m => [m a] -> m [a]

sequence' [] = return []
sequence' (m:ms) 
  = m >>=
      \ a ->
        do as <- sequence' ms
	   return (a:as)

--sequence' ms = foldr func (return ()) ms
--	where
--		func :: (Monad m) => m a -> m [a] -> m [a]
--		func m acc
--		  = do x <- m
--		       xs <- acc
--		       return (x:xs)

--sequence' ms = foldr func (return []) ms
--	where
--		func :: (Monad m) => ma -> m [a] -> m [a]
--		func m acc = m : acc

--sequence' [] = return []
--sequence' (m : ms) = return (a : as)
--	where
--	    a <- m
--	    as <- sequence' ms

--sequence' ms = foldr func (return []) ms
--	where
--		func :: (Monad m) => m a -> m [a] -> m [a]
--		func m acc
--		  = do x <- m
--		       xs <- acc
--		       return (x:xs)

--sequence' [] = return []
--sequence' (m:ms)
--  = m >> 
--      \ a ->
--        do as <- sequence' ms
--           return (a:as)

--sequence' [] = return []
--sequence' (m:ms) = m >>= \a -> 
--	as <- sequence' ms
--	return (a:as)

--sequence' [] = return []
--sequence' (m : ms) = do a <- m
--			as <- sequence' ms
--			return (a : as)

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]

filterM' _ [] = return []
filterM' p (x:xs) 
  = do flag <- p x 
       ys <- filterM' p xs
       if flag then return (x:ys) else return ys

sucessor :: Monad m => Integer -> m Integer
sucessor = \ x -> return (x+1)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]

--mapM' f as = sequence' (map f as)
mapM' f [] = return []
--mapM' f (a:as)
--  = f a >>= \ b -> mapM' f as >>= \ bs -> return (b:bs)
--mapM' f (a: as)
--  = do b <- f a
--       bs <- mapM' f as
--       return (b: bs)
mapM' f (a : as)
  = f a >>=
      \ b ->
        do bs <- mapM' f as
           return (b : bs)

liftM :: Monad m => (a -> b) -> m a -> m b

--liftM f m
--  = do x <- m
--       return (f x)

--liftM f m = m >>= \ a -> return (f a)

--liftM f m = return (f m)

liftM f m = m >>= \ a -> m >>= \ b -> return (f b)
verdadeiro x = if x == True then 1 else 0

--foldl :: (a -> b -> a) -> a -> [b] -> a

--foldl f acc [] = acc
--foldl f acc (x:xs) = f (f acc x) xs

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

foldLeftM f z []     = return z
foldLeftM f z (x:xs) = f z x >>= \ c -> foldLeftM f c xs
--foldLeftM f z (x:xs) = foldLeftM f (f z x) xs

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b

--foldr f acc [] = acc
--foldr f acc (x:xs) = f x (foldr f acc xs)

foldRightM f z [] = return z
foldRightM f z (x:xs) = (foldRightM f z xs) >>= \ y -> f x y
