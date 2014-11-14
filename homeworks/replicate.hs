import Prelude hiding (replicate)

replicate :: Int -> a -> [a]

replicate 0 _ = []
replicate n x = replicate (n - 1) x : x
