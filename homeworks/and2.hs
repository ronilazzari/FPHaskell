import Prelude hiding (and)

and :: [Bool] -> Bool

and [] = True
and (b : bs) = b && and bs 
