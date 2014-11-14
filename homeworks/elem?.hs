elem2 :: Eq a => a -> [a] -> Bool

elem2 _ [] = False
elem2 x (y : ys)
	| x == y = True
	| otherwise = elem2 x ys
