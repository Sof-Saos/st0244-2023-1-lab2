module PRF  where
import Prelude hiding (toInteger)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, sized, shrink, Gen)


data Nat = Zero | Succ Nat

----Instances
instance Eq Nat where --Compare Nats
   Zero == Zero         = True
   Succ n == Succ m     = n == m
   _      == _          = False
instance Num Nat where --Define basics algebraic operations
  fromInteger :: Integer -> Nat
  fromInteger n
    | n <= 0    = Zero
    | otherwise = Succ (fromInteger (n - 1))

  (+) :: Nat -> Nat -> Nat --Define Sum operators
  (+) = f1

  (*) :: Nat -> Nat -> Nat --Define mult operators
  (*) = f2
  
  --required syntax of the instance
  abs :: Nat -> Nat 
  abs n = n

  signum :: Nat -> Nat
  signum Zero   = Zero
  signum Succ{} = Succ Zero

  negate :: Nat -> Nat
  negate n = n  
instance Arbitrary Nat where --Generate random values
  arbitrary = sized $ \n -> genNat n --random generation of values depending on n
    where
      genNat :: Int -> Gen Nat --Funtion that generate a random value of Nat
      genNat 0 = return Zero
      genNat n = oneof [return Zero, Succ <$> genNat (n - 1)]
  shrink Zero     = [] --No values are generated because there are no values smaller than zero
  --If the value generated is Succ then it generates smaller values than the previous one
  shrink (Succ n) = n : shrink n 
instance Ord Nat where -- Compare Nats values to order them
  Zero <= _ = True
  Succ m <= Succ n = m <= n
  _ <= _ = False
instance Show Nat where --Converts to strings
  show n = show (toInteger n)
----Funtiones
f1 :: Nat -> Nat -> Nat -- Sum
f1 m n = recNat n (\ _ y -> Succ y) m

f2 :: Nat -> Nat -> Nat --Multiplication
f2 m = recNat Zero (\_ acc -> f2 acc m)

f3 :: Nat -> Nat -> Nat --Pow
f3 m n = recNat (Succ Zero) (\_ y -> f2 m y) n

f4 :: Nat -> Nat -> Nat -- Sum using the result of f1
f4 m n = recNat n (\ _ acc -> f1 m acc) m

f5 :: Nat -> Nat -> Nat --Id
f5 m n = recNat m (\_ acc -> Succ acc) n

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

toInteger :: Nat -> Integer --Converts to Integer
toInteger = recNat 0 (\_ acc -> acc + 1)

