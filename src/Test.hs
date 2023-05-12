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
prop_f3 n _ = f3 n Zero === Succ Zero

---------------------------------------------------------
main :: IO()
main = do
    quickCheck prop_f1
    quickCheck prop_f2
    quickCheck prop_f3
    -- quickCheck prop_f4
    -- quickCheck prop_f5