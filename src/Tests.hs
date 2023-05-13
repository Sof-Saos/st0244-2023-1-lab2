import PRF
import Numeric.Natural ( Natural )
import Test.QuickCheck
  ( Arbitrary ( arbitrary, shrink )
  , Property
  , arbitrarySizedNatural
  , classify
  , collect
  , quickCheck
  , shrinkIntegral
  , (===)
  , sized
  , oneof
  , (==>)
  , verboseCheck
  )

----------Instances for QuickCheck----------------------

instance Arbitrary Nat where
  arbitrary = sized $ \n -> genNat n
    where
      genNat 0 = return Zero
      genNat n = oneof [return Zero, Succ <$> genNat (n - 1)]
  shrink Zero     = []
  shrink (Succ n) = n : shrink n
 
instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral

instance Ord Nat where
  Zero <= _ = True
  Succ m <= Succ n = m <= n
  _ <= _ = False

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