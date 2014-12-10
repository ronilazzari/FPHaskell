--data Estringue = ['a'..'z']

import Test.QuickCheck

--instance Arbitrary  Estringue where
--	arbitrary = ['a'..'z']

removeone :: Eq a => a -> [a] -> [a]

removeone x [] = []
removeone x (y:ys)
	| x == y = ys
	| otherwise = y : removeone x ys

isChoice :: Eq a => [a] -> [a] -> Bool

isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice (removeone x xs) ys


split :: [Bool] -> [([Bool], [Bool])]

split [] = []
split [_] = []
split (x:xs) = ([x], xs):[(x:ls, rs) | (ls, rs) <- split xs]

prop_split :: [Bool] -> Bool

prop_split xs = and [ (fst ys ++ snd ys) == xs | ys <- split xs]
