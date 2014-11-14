--import Prelude hiding (all)

--all p xs = and (map p xs)
--all p = and . map p
--all p = not . any (not . p)
--all p xs = foldl (&&) True (map p xs)
--all p = foldr (&&) True . map p -- esse aqui eu não entendi!!!

--import Prelude hiding (any)

--any p = map p . or
--any p = or . map p
--any p xs = length (filter p xs) > 0
--any p = not . null . dropWhile (not . p)
--any p xs = not (all (\ x -> not (p x)) xs)
--any p xs = foldr (\ x acc -> (p x) || acc) False xs

--import Prelude hiding (takeWhile)

--takeWhile _ [] = []
--takeWhile p (x:xs)
--	| p x = x:takeWhile p xs
--	| otherwise = []

--import Prelude hiding (dropWhile)

--dropWhile _ [] = []
--dropWhile p (x:xs)
--	| p x = dropWhile p xs
--	| otherwise = x:xs

--import Prelude hiding (map)

--map f = foldr (\ x xs -> xs ++ [f x]) [] esse resultado fica invertido!
--map f = foldl (\ xs x -> xs ++ [f x]) []

--import Prelude hiding (filter)

--filter p = foldr (\ x xs -> if p x then x:xs else xs) []

--dec2int :: [Integer] -> Integer

--dec2int = foldl (\ x y -> 10 * x + y) 0

--import Prelude hiding (curry)

--curry f = \ x y -> f (x,y)

--import Prelude hiding (uncurry)

--soma :: Integer -> Integer -> Integer

--soma x y = x + y

--uncurry f = \ (x,y) -> f x y

--f = \ x -> x
--f (g, x) = g x
--f x = x : f x
import Prelude hiding (iterate)

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]

unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)

iterate f = unfold (const False) f f
--type Bit = Int

--int2bin:: Int -> [Bit]

--int2bin 0 = []
--int2bin n = n `mod` 2 : int2bin (n `div` 2)

--int2bin = unfold (== 0) (`mod` 2) (`div` 2)

--chop8 :: [Bit] -> [[Bit]]

--chop8 = unfold null (take 8) (drop 8)
--chop8 [] = []
--chop8 bits = take 8 bits : chop8 (drop 8 bits)

--map f = unfold null (f . head) tail


