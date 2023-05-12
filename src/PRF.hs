module PFR (f1, f2, f3, f4, f5) where
data Nat = Zero | Succ Nat

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

f1 :: Nat -> Nat -> Nat
f1 m n = recNat n (\ _ y -> Succ y) m

f2 :: Nat -> Nat -> Nat
f2 m = recNat Zero (\n acc -> f2 acc m)

