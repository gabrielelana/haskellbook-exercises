{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Meetup.Applicatives where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative ( Applicative(liftA2) )


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkItWorks :: SpecWith ()
checkItWorks = it "works" $ 1 + 1 `shouldBe` 2

-- NOTE: use of the `do` notation without a monad
-- NOTE: use of forall + Scopedtypevariables + TypeApplications

-- identity: pure id <*> x == x
identityLaw :: (Applicative f, Eq (f x)) => f x -> Bool
identityLaw x = (pure id <*> x) == x
-- identityLaw x = (==)
--   do id <$> x
--   do x

-- composition: pure (.) <*> u <*> w <*> z == u <*> (w <*> z)
compositionLaw :: (Applicative f, Eq (f c)) => Fun b c -> Fun a b -> f a -> Bool
compositionLaw (Fn u) (Fn w) z = (pure (.) <*> u' <*> w' <*> z) == (u' <*> (w' <*> z))
  where u' = pure u
        w' = pure w
-- compositionLaw u w z = (==)
--   do (.) <$> u <*> w <*> z
--   do u <*> (w <*> z)

-- homomorphism: pure f <*> pure x == pure (f x)
homomorphismLaw :: forall f a b. (Applicative f, Eq (f b)) => Fun a b -> a -> Bool
homomorphismLaw (Fn f) x = (pure f <*> pure x) == pure @f (f x)
-- homomorphismLaw f x = (==)
--   do f <$> pure x
--   do pure @f $ f x

-- interchange: u <*> pure y == pure ($ y) <*> u
interchangeLaw :: forall f a b. (Applicative f, Eq (f b)) => (Fun a b) -> a -> Bool
interchangeLaw (Fn u) y = (u' <*> pure y) == (pure ($ y) <*> u')
  where u' = pure @f u
-- interchangeLaw u y = (==)
--   do u <*> pure y
  -- do pure ($ y) <*> u

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Bin
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Bin a = Bin (Bin a) (Bin a) | Leaf a deriving (Functor, Eq, Show)

instance Applicative Bin where
  pure = Leaf

  Leaf f  <*> r           = f <$> r
  -- Leaf f  <*>    Leaf x   = Leaf $ f x
  -- Leaf f  <*>    Bin l r  = Bin (f <$> l) (f <$> r)
  Bin l r <*> x@(Leaf _)  = Bin (l <*> x) (r <*> x)
  f       <*>   (Bin l r) = Bin (f <*> l) (f <*> r)

instance Arbitrary a => Arbitrary (Bin a) where
  arbitrary = do
    x <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [ (3, return $ Leaf x)
              , (1, return $ Bin l r)
              ]

checkBin :: SpecWith ()
checkBin =
  describe "Bin" $ do
    it "examples" $ do
      Leaf (+ 1) <*> Leaf 2 `shouldBe` Leaf 3
      Leaf (+ 1) <*> Bin (Leaf 1) (Leaf 2) `shouldBe` Bin (Leaf 2) (Leaf 3)
    it "applicative identity" $
      property (identityLaw @Bin @Int)
    it "applicative composition" $
      property (compositionLaw @Bin @Int @Int @Int)
    it "applicative homomorphism" $
      property (homomorphismLaw @Bin @Int @Int)
    it "applicative interchange" $
      property (interchangeLaw @Bin @Int @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- RoseL
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data RoseL a = RoseL a [RoseL a] deriving (Functor, Eq, Show)

instance Applicative RoseL where
  pure x = RoseL x []

  (RoseL f fs) <*> r@(RoseL x xs) = RoseL (f x) $ ((f <$>) <$> xs) <> ((<*> r) <$> fs)

  -- alternative
  -- (RoseL f fs) <*> r@(RoseL x xs) = RoseL (f x) $ ((<*> r) <$> fs) <> ((f <$>) <$> xs)

  -- (RoseL f []) <*> (RoseL x []) = RoseL (f x) []
  -- (RoseL f []) <*> (RoseL x xs) = RoseL (f x) $ (fmap . fmap) f xs
  -- (RoseL f fs) <*> (RoseL x []) = RoseL (f x) $ (fmap . fmap) ($ x) fs
  -- l            <*> (RoseL x xs) = let (RoseL y ys) = l <*> (RoseL x []) in
  --                                   (RoseL y (ys <> ((\x -> l <*> x) <$> xs)))

instance Arbitrary a => Arbitrary (RoseL a) where
  arbitrary = do
    a <- arbitrary
    r1 <- arbitrary
    r2 <- arbitrary
    -- r3 <- arbitrary
    frequency [ (3, return $ RoseL a [])
              , (1, return $ RoseL a [r1])
              , (1, return $ RoseL a [r1, r2])
              -- , (1, return $ RoseL a [r1, r2, r3])
              ]

checkRoseL :: SpecWith ()
checkRoseL =
  describe "RoseL" $ do
    it "examples" $ do
      RoseL (+1) [] <*> RoseL 1 [] `shouldBe` RoseL 2 []
      RoseL (+1) [] <*> RoseL 1 [RoseL 2 []] `shouldBe` RoseL 2 [RoseL 3 []]
    it "applicative identity" $
      property (identityLaw @RoseL @Int)
    it "applicative composition" $
      property (compositionLaw @RoseL @Int @Int @Int)
    it "applicative homomorphism" $
      property (homomorphismLaw @RoseL @Int @Int)
    it "applicative interchange" $
      property (interchangeLaw @RoseL @Int @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- Compose'
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Compose' f g a = Compose' (f (g a)) deriving (Eq, Show, Functor)

instance (Applicative f, Applicative g) => Applicative (Compose' f g) where
  pure = Compose' . pure . pure

  (Compose' f) <*> (Compose' x) = Compose' $ liftA2 (<*>) f x
  -- (Compose' f) <*> (Compose' x) = Compose' $ (liftA2 . liftA2) ($) f x

instance (Applicative f, Applicative g, Arbitrary a) => Arbitrary (Compose' f g a) where
  arbitrary = pure <$> arbitrary
  -- arbitrary = do
    -- a <- arbitrary
    -- return $ pure a

checkCompose' :: SpecWith ()
checkCompose' =
  describe "Compose'" $ do
    it "examples" $ do
      Compose' (Just [(+ 1)]) <*> Compose' (Just [1]) `shouldBe` Compose' (Just [2])
      Compose' (Just [(+ 1)]) <*> Compose' (Just [1, 2]) `shouldBe` Compose' (Just [2, 3])
      Compose' (Just [(+ 1), (+ 2)]) <*> Compose' (Just [1, 2]) `shouldBe` Compose' (Just [2, 3, 3, 4])
      -- NOTE: cannot do the following ???
      -- Compose' (Just []) <*> (Compose' (Just [1, 2])) `shouldBe` Compose' (Just [])
      -- Compose' (+ 1) [] <*> Compose' 1 [Compose' 2 []] `shouldBe` Compose' 2 [Compose' 3 []]
    it "applicative identity" $
      property (identityLaw @(Compose' Maybe []) @Int)
    it "applicative composition" $
      property (compositionLaw @(Compose' Maybe []) @Int @Int @Int)
    it "applicative homomorphism" $
      property (homomorphismLaw @(Compose' Maybe []) @Int @Int)
    it "applicative interchange" $
      property (interchangeLaw @(Compose' Maybe []) @Int @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- (*>) && (<*)
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- keepRight
kr :: Applicative f => f a -> f b -> f b
-- kr l r = const <$> r <*> l
kr = liftA2 (flip const)

-- keepLeft
kl :: Applicative f => f a -> f b -> f a
-- kl l r = const <$> l <*> r
kl = liftA2 const

checkApplicativeOp :: SpecWith ()
checkApplicativeOp = do
  describe "applicative operator *>" $
    it "must behave like the one from Control.Applicative" $ do
      -- NOTE: cannot be replaced with `undefined` ???
      kr (Just 1) (Just 2) `shouldBe` Just 1 *> Just 2
      kr Nothing (Just 2) `shouldBe` Nothing *> Just 2
      kr (Just 1) (Nothing @Int) `shouldBe` (Just 1) *> Nothing
  describe "applicative operator <*" $
    it "must behave like the one from Control.Applicative" $ do
      kl (Just 1) (Just 2) `shouldBe` Just 1 <* Just 2
      kl (Nothing @Int) (Just 2) `shouldBe` Nothing <* Just 2
      kl (Just 1) (Nothing @Int) `shouldBe` (Just 1) <* Nothing

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Reader r a = Reader {runReader :: r -> a} deriving (Functor)

instance Applicative (Reader r) where
  pure = Reader . const

  -- (Reader rf) <*> (Reader rx) = Reader (\r -> rf r (rx r))
  Reader rf <*> Reader rx = Reader $ rf <*> rx

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype State s a = State {runState :: s -> (a, s)} deriving (Functor)

instance Applicative (State s) where
  pure = State . (,)

  State sfab <*> State sa = State (\s -> let (fab, s') = sfab s in
                                         let (a, s'') = sa s' in
                                           (fab a, s''))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Writer w a = Writer {runWriter :: (a, w)} deriving (Functor)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)

  Writer (fab, w1) <*> Writer (a, w2) = Writer (fab a, w1 <> w2)


isVowel :: Char -> Bool
isVowel = flip elem ['a', 'e', 'i', 'o', 'u']

diff :: Reader String Int
diff = (-) <$> vowel <*> consonant
  where vowel = Reader $ length . filter isVowel
        consonant = Reader $ length . filter (not . isVowel)

appDiff = runReader diff "Ci vediamo la prossima volta"


readAndSum :: IO Int
readAndSum = (+) <$> readLn <*> readLn

readAndProd :: IO (Int, Int, Int)
readAndProd = (,,) <$> readAndSum <*> readAndSum <*> readAndSum

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check :: IO ()
check = do
  hspec $ do
    checkItWorks
    checkBin
    checkRoseL
    checkCompose'
    checkApplicativeOp
