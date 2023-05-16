module PRF  where
import Prelude hiding (toInteger)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, sized, shrink, Gen)

data Nat = Zero | Succ Nat
data Natural = NatZero | NatSucc Natural
----Instances
instance Eq Natural where
  (==) :: Natural -> Natural -> Bool
  NatZero == NatZero = True
  NatSucc n == NatSucc m = n == m
  _ == _ = False
instance Num Natural where
  fromInteger :: Integer -> Natural
  fromInteger n
    | n <= 0 = NatZero
    | otherwise = NatSucc (fromInteger (n - 1))

  (+) :: Natural -> Natural -> Natural
  NatZero + n = n
  NatSucc m + n = NatSucc (m + n)

  (*) :: Natural -> Natural -> Natural
  NatZero * _ = NatZero
  NatSucc m * n = n + (m * n)

  abs :: Natural -> Natural
  abs n = n

  signum :: Natural -> Natural
  signum NatZero = NatZero
  signum _ = NatSucc NatZero

  negate :: Natural -> Natural
  negate n = n

(^) :: Natural -> Natural -> Natural --Define a pow operator to be used as a comparator in the prop
m ^ n = natToNatural (f3 m' n')
  where
      m', n' :: Nat
      m' = naturalToNat m
      n' = naturalToNat n

instance Num Nat where --Define basics algebraic operations
  fromInteger :: Integer -> Nat
  fromInteger n
    | n <= 0    = Zero
    | otherwise = Succ (fromInteger (n - 1))
  (+) :: Nat -> Nat -> Nat --Define Sum operator
  (+) = f1

  (*) :: Nat -> Nat -> Nat --Define mult operator
  (*) = f2

  abs :: Nat -> Nat
  abs n = n
  signum :: Nat -> Nat
  signum Zero   = Zero
  signum Succ{} = Succ Zero
  negate :: Nat -> Nat
  negate n = n
instance Arbitrary Natural where
  arbitrary :: Gen Natural
  arbitrary = sized $ \n -> genNatural n
    where
      genNatural :: Int -> Gen Natural
      genNatural 0 = return NatZero
      genNatural n = oneof [return NatZero, NatSucc <$> genNatural (n - 1)]

  shrink :: Natural -> [Natural]
  --No values are generated because there are no values smaller than zero
  shrink NatZero = []
  --If the value generated is Succ then it generates smaller values than the previous one
  shrink (NatSucc n) = n : shrink n
instance Show Natural where
  show :: Natural -> String
  show NatZero = "NatZero"
  show (NatSucc n) = "NatSucc (" ++ show n ++ ")"
instance Show Nat where --Converts to strings
  show n = show (toInteger n)
----Funtiones
f1 :: Nat -> Nat -> Nat -- Sum
f1 m n = recNat n (\ _ y -> Succ y) m

f2 :: Nat -> Nat -> Nat -- Multiplication
f2 m = recNat Zero (\_ acc -> acc + m)

f3 :: Nat -> Nat -> Nat --Pow
f3 m n = recNat (Succ Zero) (\_ y -> f2 m y) n

f4 :: Nat -> Nat -> Nat --Predecesor
f4 Zero _ = Zero
f4 (Succ n) m = recNat m (\_ prev -> prev) n

f5 :: Nat -> Nat -> Nat --Ck(x) = k
f5 k _ = recNat k (\_ acc -> acc) k

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

toInteger :: Nat -> Integer --Converts to Integer
toInteger = recNat 0 (\_ acc -> acc + 1)

natToNatural :: Nat -> Natural
natToNatural Zero = NatZero
natToNatural (Succ n) = NatSucc (natToNatural n)

naturalToNat :: Natural -> Nat
naturalToNat NatZero = Zero
naturalToNat (NatSucc n) = Succ (naturalToNat n)