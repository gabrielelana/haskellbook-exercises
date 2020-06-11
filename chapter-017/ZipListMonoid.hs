module ZipListMonoid where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--  This will not work intentionally
instance Semigroup a => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList []

-- The book reports the following Arbitrary instances but
-- in the ghc version I'm using they are already provided

-- instance Arbitrary a => Arbitrary (ZipList a) where
--   arbitrary = ZipList <$> arbitrary

-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = Sum <$> arbitrary

-- instance Eq a => EqProp (ZipList a) where
--   (=-=) = eq
