sortit [] = []
sortit (x:xs) = sortit larger ++ [x] ++ sortit smaller
	where smaller = [a | a <- xs, a < x ]
	      larger = [b | b <- xs, b > x]
