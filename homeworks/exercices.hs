import Prelude hiding (replicate)

sumsquares n = sum [x^2 | x <- [1..n]]

replicate n value = [value | _ <- [1..n]]

pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <-[1..n], x^2 + y^2 == z^2]

factors n = [x | x <- [1..n], n `mod` x == 0]

perfect n = [x | x <- [1..n], sum (init (factors x)) == x]

scalarproduct xs ys = sum [x'*y' | (x', y') <- zip xs ys]

faz xs = 1 : [ x + 1 | x <- xs]

divides a b = a `mod` b == 0

divisors x = [d | d <- [1..x], x `divides` d]

find k t = [v | (k', v) <- t, k == k']

positions x xs = find x (zip xs [0..n])
	where n = length xs - 1
