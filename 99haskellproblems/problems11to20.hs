--------------------
--  Problem 11    --
--------------------

--------------------
--  Problem 12    --
--------------------

--------------------
--  Problem 13    --
--------------------

--------------------
--  Problem 14    --
--------------------
duplicate :: [a] -> [a]

duplicate [] = []

duplicate (x:xs) = x:x:(duplicate xs)

--------------------
--  Problem 15    --
--------------------
repli :: Int -> a -> [a]

repli  0 x = []
repli  1 x = [x]
repli  n x = x:(repli (n - 1) x)

replicate' :: Int -> [a] -> [a]

replicate' _ [] = []
replicate' n (x:xs) = repli n x ++ replicate' n xs

--------------------
--  Problem 16    --
--------------------
dropEvery :: Int -> [a] -> [a]

dropEvery n (x:xs)
	| n > length (x:xs) = error "Index is greater than the number of elements of list"
	| n == 0 = (x:xs)
	| n == 1 = xs
	| otherwise = [x] ++ dropEvery (n - 1) xs

--------------------
--  Problem 17    --
--------------------

--------------------
--  Problem 18    --
--------------------

--------------------
--  Problem 19    --
--------------------

--------------------
--  Problem 20    --
--------------------

