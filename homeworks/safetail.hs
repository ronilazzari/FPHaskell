safetail :: [a] -> [a]

{- função safetail feita com condicionais
-}
safetail xs = if null xs then [] else tail xs


{- função safetail feitas com guardas
safetail xs | null xs = []
	    | otherwise = tail xs
-}

{-
safetail [] = []
safetail xs = tail xs
-}

{-
safetail [] = []
safetail (_ : xs) = xs
-}

{-
safetail xs
	| length xs == 0 = []
	| otherwise = tail xs
-}
