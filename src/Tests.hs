import PRF
import Test.QuickCheck (quickCheck)

prop_f1 :: Natural -> Natural -> Bool--Commutative sum
prop_f1 m n = natToNatural (f1 m' n') == m + n
   where
     m', n' :: Nat
     m' = naturalToNat m
     n' = naturalToNat n

prop_f2 :: Natural -> Natural -> Bool --conmutative multiplication
prop_f2 m n = natToNatural (f2 m' n') == m * n
   where
     m', n' :: Nat
     m' = naturalToNat m
     n' = naturalToNat n

prop_f3 :: Natural -> Natural -> Bool --compares the result of the function f3 power with one from the same Num Natural scheme but for power
prop_f3 m n = natToNatural (f3 m' n') == m PRF.^ n
    where
     m', n' :: Nat
     m' = naturalToNat m
     n' = naturalToNat n

-- prop_f4 :: Natural -> Natural -> Bool
-- prop_f4 n m = n == expected
--   where
--     natN = naturalToNat n
--     natM = naturalToNat m
--     expected = natToNatural (f4 natN natM)
--prop_f4 n m = n == natToNatural (f4 (naturalToNat n) (naturalToNat m))

prop_f5 :: Natural -> Natural -> Bool --compares if the constante funtion works
prop_f5 m n = natToNatural (f5 m' n') == m
   where
     m' = naturalToNat m
     n' = naturalToNat n

--Main
main :: IO()
main = do
      quickCheck prop_f1
      quickCheck prop_f2
      quickCheck prop_f3
      --quickCheck prop_f4
      quickCheck prop_f5
