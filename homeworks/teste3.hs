mylast xs = head (reverse xs) 

mylast2 xs = xs !! (length xs - 1)

myinit xs = reverse (tail (reverse xs))

myinit2 xs = reverse (drop 1 (reverse xs))
