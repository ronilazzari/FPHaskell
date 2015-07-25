--------------------
--   Problem 9    --
--------------------
pack' :: Eq a => [a] -> [[a]] -> [[a]]

pack' xs ys
	     | xs == [] = ys
	     | otherwise  = pack' xs' ys'
	     	where
	     	     xs' = dropWhile ((==) (head xs)) xs
		     ys' = (ys ++ [(takeWhile ((==) (head xs)) xs)])

pack :: Eq a => [a] -> [[a]]

pack xs = pack' xs []

--------------------
--  Problem 10    --
--------------------
encode :: Eq a => [a] -> [(Int, a)]

encode xs = [((length x), (head x)) | x <- (pack xs)]

--------------------
--  Problem 11    --
--------------------
data RunLengthEncoding = Multiple Int Char | Single Char
					deriving (Eq,Show)

encodeModified :: [Char] -> [RunLengthEncoding]

encodeModified xs = [if (fst x) == 1 then (Single (snd x)) else (Multiple (fst x) (snd x)) | x <- (encode xs)]

--------------------
--  Problem 12    --
--------------------
decode :: RunLengthEncoding -> [Char]

decode (Single x) = repli 1 x
decode (Multiple number character) = repli number character

decodeModified' :: [RunLengthEncoding] -> [Char] -> [Char]

decodeModified' xs ys
		      | xs == []  = ys
		      | otherwise = decodeModified' xs' ys'
		      	where
		      		xs' = tail xs
		      		ys' = ys ++ (decode (head xs))

decodeModified :: [RunLengthEncoding] -> [Char]

decodeModified xs = decodeModified' xs []

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

