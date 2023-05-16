module PRF  where
import Prelude hiding (toInteger)

data Nat = Zero | Succ Nat
----Funtiones
f1 :: Nat -> Nat -> Nat -- Sum
f1 m n = recNat n (\ _ y -> Succ y) m

f2 :: Nat -> Nat -> Nat -- Multiplication
f2 m n = recNat Zero (\_ acc -> f1 m acc) n

f3 :: Nat -> Nat -> Nat --Pow
f3 m n = recNat (Succ Zero) (\_ y -> f2 m y) n

f4 :: Nat -> Nat  --Predecesor
f4 Zero = Zero
f4 (Succ n) = recNat n (\ _ y -> y ) Zero

f5 :: Nat -> Nat -> Nat --Ck(x) = k
f5 k _ = recNat k (\_ acc -> acc) k

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)