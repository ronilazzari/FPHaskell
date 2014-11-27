--------------------
--   Problem 1    --
--------------------
mylast :: [a] -> a

mylast []     = error "Empty list does not have elements!"
mylast [x]    = x
mylast (x:xs) = mylast xs

--------------------
--   Problem 2    --
--------------------
myButLast :: [a] -> a

myButLast []  = error "Empty list does not have elements!"
myButLast [x] = error "There is no element before the first one"
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

--------------------
--   Problem 3    --
--------------------
elementAt :: [a] -> Integer -> a

elementAt []     _   = error "Empty list does not have elements!"
elementAt _      0   = error "Index must be equal to or greater than 1"
elementAt (x:xs) n   = if (n == 1) then x else elementAt xs (n - 1)

--------------------
--   Problem 4    --
--------------------
myLength :: [a] -> Integer

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--------------------
--   Problem 5    --
--------------------
myReverse :: [a] -> [a]

myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

--------------------
--   Problem 6    --
--------------------
isPalindrome :: Eq a => [a] -> Bool

isPalindrome [] = True
isPalindrome xs = xs == myReverse xs

--------------------
--   Problem 7    --
--------------------
-- to be implemented

--------------------
--   Problem 8    --
--------------------
compress :: String -> String

compress [] = []
compress [x] = [x]
compress (x:xs)
		| x == head xs = compress xs
		| otherwise = [x] ++ compress xs

compress' :: Eq a => [a] -> [a]

compress' [] = []
compress' [x] = [x]
compress' (x:xs)
		| x == head xs = compress' xs
		| otherwise = [x] ++ compress' xs

--------------------
--   Problem 9    --
--------------------
pack :: Eq a => [a] -> [[a]]

pack []     = []
pack [x]    = [x:[]]
pack (x:y:zs)
	   | x == y = [x:y:[]] ++ pack zs
	   | otherwise = [x:[]] ++ [y:[]] ++ pack zs
-- this one is wrongly implemented, must correct it

--------------------
--  Problem 10    --
--------------------
-- to be implemented
