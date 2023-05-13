module PRF  where
import Prelude hiding (toInteger)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, sized, shrink, Gen)


data Nat = Zero | Succ Nat

----Instancias 
instance Eq Nat where
   Zero == Zero         = True
   Succ n == Succ m     = n == m
   _      == _          = False
instance Num Nat where
  fromInteger :: Integer -> Nat
  fromInteger n
    | n <= 0    = Zero
    | otherwise = Succ (fromInteger (n - 1))

  (+) :: Nat -> Nat -> Nat
  (+) = f1

  (*) :: Nat -> Nat -> Nat
  (*) = f2

  abs :: Nat -> Nat
  abs n = n

  signum :: Nat -> Nat
  signum Zero   = Zero
  signum Succ{} = Succ Zero

  negate :: Nat -> Nat
  negate n = n  
instance Arbitrary Nat where
  arbitrary = sized $ \n -> genNat n
    where
      genNat :: Int -> Gen Nat
      genNat 0 = return Zero
      genNat n = oneof [return Zero, Succ <$> genNat (n - 1)]
  shrink Zero     = []
  shrink (Succ n) = n : shrink n
instance Ord Nat where
  Zero <= _ = True
  Succ m <= Succ n = m <= n
  _ <= _ = False
instance Show Nat where
  show n = show (toInteger n)
----Funciones
f1 :: Nat -> Nat -> Nat --Suma
f1 m n = recNat n (\ _ y -> Succ y) m

f2 :: Nat -> Nat -> Nat --Multiplicación
f2 m = recNat Zero (\_ acc -> f2 acc m)
f3 :: Nat -> Nat -> Nat --Potencia
f3 m n = recNat (Succ Zero) (\_ y -> f2 m y) n

f4 :: Nat -> Nat -> Nat -- Suma recursiva que utiliza f1
f4 m n = recNat n (\ _ acc -> f1 m acc) m

-- f5 :: Nat -> Nat -> Nat
-- f5 

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)
toInteger :: Nat -> Integer
toInteger = recNat 0 (\_ acc -> acc + 1)

