import PRF
import Test.QuickCheck
  ( Property
  , quickCheck
  , (===)
  , (==>)
  )
  --, verboseCheck
--------------------Properties----------------------

prop_f1 :: Nat -> Nat -> Property
prop_f1 x y = (x + y) === (y + x)

prop_f2 :: Nat -> Nat -> Property
prop_f2 x y = (x * y) === (y * x)

prop_f3 :: Nat -> Nat -> Property
prop_f3 x _ = f3 x Zero === Succ Zero

prop_f4 :: Nat -> Nat -> Property
prop_f4 m n = (m > Zero && n > Zero) ==> f4 m n > m

---------------------------------------------------------
main :: IO()
main = do
    quickCheck prop_f1
    quickCheck prop_f2
    quickCheck prop_f3
    quickCheck prop_f4
    -- quickCheck prop_f5

    --verbose para ver que casos usa