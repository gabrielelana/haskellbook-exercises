module ZipListMonoid2 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- This will not work intentionally

instance Semigroup a => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty

-- instance Eq a => EqProp (ZipList a) where
--   (=-=) = eq
