import PRF
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Instances.Natural ()
import Numeric.Natural (Natural)

natToNatural :: Nat -> Natural
natToNatural Zero = 0
natToNatural (Succ n) = 1 + natToNatural n

naturalToNat :: Natural -> Nat
naturalToNat 0 = Zero
naturalToNat n = Succ (naturalToNat (n - 1))

prop_f1 :: Natural -> Natural -> Bool
prop_f1 m n = natToNatural (f1 m' n') == mNat + nNat
  where
    m', n' :: Nat
    m' = naturalToNat m
    n' = naturalToNat n
    mNat = natToNatural m'
    nNat = natToNatural n'

prop_f2 :: Natural -> Natural -> Bool --conmutative multiplication
prop_f2 m n = natToNatural (f2 m' n') == m * n
   where
     m', n' :: Nat
     m' = naturalToNat m
     n' = naturalToNat n

prop_f3 :: Natural -> Natural -> Bool
prop_f3 m n = natToNatural (f3 m' n') == mNat ^ nNat
  where
    m', n' :: Nat
    m' = naturalToNat m
    n' = naturalToNat n
    mNat = natToNatural m'
    nNat = natToNatural n'


prop_f4 :: Natural -> Bool
prop_f4 n = if n == 0
          then True
          else natToNatural (f4 n') == n - 1
     where
          n' :: Nat
          n' = naturalToNat n

prop_f5 :: Natural -> Natural -> Bool
prop_f5 m n = natToNatural (f5 m' n') == mNat
  where
    m', n' :: Nat
    m' = naturalToNat m
    n' = naturalToNat n
    mNat = natToNatural m'

--Main
main :: IO()
main = do
      quickCheck prop_f1
      quickCheck prop_f2
      quickCheck prop_f3
      quickCheck prop_f4
      quickCheck prop_f5
