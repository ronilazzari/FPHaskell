halve1 :: [ a ] -> ([ a ], [ a ])

halve1 xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)
