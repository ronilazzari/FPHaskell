import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

natToInteger :: Nat -> Integer

natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

--natToInteger (Succ n) = 1 + natToInteger n
--natToInteger Zero = 0

--natToInteger Zero = 1
--natToInteger (Succ n) = (1 + natToInteger n) - 1

--natToInteger = head . m
--	where m Zero = [0]
--	      m (Succ n) = [sum [x | x <- (1: m n)]]

--natToInteger = \ n -> genericLength [c | c <- show n, c == 'S']

--natToInteger = \ n -> length [c | c <- show n, c == 'S']

integerToNat :: Integer -> Nat

integerToNat 0 = Zero
integerToNat n = Succ (integerToNat (n - 1))

--integerToNat n = product [(unsafeCoerce c) :: Integer | c <- show n]

--integerToNat n = let m = integerToNat (n - 1) in Succ m
--integerToNat 0 = Zero

--integerToNat = head . m
--	where {
--	      ; m 0 = [0]
--	      ; m (n + 1) = [sum [x | x <- (1 : m n)]]
--	      }

--integerToNat = \ n -> genericLength [c | c <- show n, isDigit c]

--integerToNat (n + 1) = Succ (integerToNat n)
--integerToNat 0 = Zero

add :: Nat -> Nat -> Nat

--add (Succ m) n = Succ (add n m)
--add Zero n = n

add Zero n = n
add (Succ m) n = Succ (add n m)

mult :: Nat -> Nat -> Nat

--mult Zero Zero = Zero
--mult m (Succ n) = add m (mult m n)

mult m Zero = Zero
mult m (Succ n) = add m (mult m n)
