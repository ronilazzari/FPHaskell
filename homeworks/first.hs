evens xs = [x | x <- xs, even x]

squares :: Integer -> [Integer]

squares n 
	| n == 0 = [] 
	| otherwise = [x * x | x <- [1..n]]

sumSquares :: Integer -> Integer

sumSquares n = sum (squares n)
