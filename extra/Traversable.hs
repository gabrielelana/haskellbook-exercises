{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module TraversableExtra where

-- `<$>` walks the structure, change the content by applying a function and rebuild the structure
-- `foldMap` walks the structure, collects the results of a function application and returns the result without structure

import Control.Applicative
import Data.Monoid
import Data.Tuple
import Control.Monad.State

deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = if x < 0 then Nothing else Just x

-- We want to implement a function that will return the original list only if it doesn't contain negative numbers

-- rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]

-- deleteIfNegative <$> [1, 2, -3, 4] == [Just 1, Just 2, Nothing, Just 4]

-- seems close but we still need to combine all the `Maybe` structures, and that's why Traversable requires Applicatives

-- class (Functor t, Foldable t) => Traversable (t :: * -> *) where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   sequenceA :: Applicative f => t (f a) -> f (t a)

-- Therefore

rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives = traverse deleteIfNegative

-- X: give the `Tree` data structure a `Traversable` instance

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show, Functor)

instance Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f b (Leaf a)       = f a b
  foldr f b (Branch t1 t2) = foldr f (foldr f b t2) t1

instance Traversable Tree where
  -- traverse :: (Applicative f, Foldable t) => (a -> f b) -> t a -> f (t b)
  traverse f (Leaf a)       = Leaf <$> f a
  traverse f (Branch t1 t2) = Branch <$> traverse f t1 <*> traverse f t2

-- `traverse` adds an extra layer of context on top of the structure
-- `traverse` allows for _effectful traversals_ which produce an overall effect

-- X: consider a representation of matrices as nested lists, with
-- nested list being rows. Use traversable to implement `transpose`

-- My naive solution

transpose :: [[a]] -> [[a]]
transpose xs = let ts = sequenceA xs
                   l = length (head xs)
                   diagonal = (\x -> x * l + x) <$> [0..(l - 1)]
               in (ts !!) <$> diagonal

-- The correct solution was to search for an Applicative on list that
-- doesn't treat lists as choices that needs to be combined but zips
-- them

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

-- X: Explain `traverse mappend`
-- It will use the `Applicative` of function by partially applying
-- mappend to all the values in the structure and returns a function
-- (the effect) that given a value (of the same type contained in the
-- structure and for which there's an instance of Monoid) will append
-- the value to all the values in the original structure

-- X: `mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)

mapAccumL' :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL' f a tb = let (Last (Just a), tc) = traverse (f' (Last (Just a))) tb in (a, tc)
  where f' (Last (Just a)) = \b -> let (a, c) = f a b in (Last (Just a), c)

-- Using State

-- mapAccumLS'' :: (b -> a -> (a, c)) -> t b -> (a -> (a, t c))
-- mapAccumLS'' :: (b -> StateL a c) -> t b -> StateL a (t c)
-- mapAccumLS'' = traverse

-- Where StateL is like State but with state and value swapped in the tuple
-- newtype State  s a = State  { runState  :: s -> (a, s) }
-- newtype StateL s a = StateL { runStateL :: s -> (s, a) }

-- We use `swap` with `State` instead of defining `StateL` like base
-- does in `Data.Traversable`

-- BTW https://en.wikibooks.org/wiki/Haskell/Solutions/Traversable is wrong

mapAccumLS :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumLS f a t = swap $ runState (traverse (\b -> state $ \a -> swap $ f a b) t) a
