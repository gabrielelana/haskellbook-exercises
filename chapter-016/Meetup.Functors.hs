{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE TypeApplications #-}

module Meetup.Functors where

import Data.List (intercalate)
-- import Data.Monoid
-- import Data.Proxy
import Test.QuickCheck
import Test.Hspec

{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Use tuple-section" #-}
{-# ANN module "HLint: ignore Functor law" #-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Impossible
data Ordering' = LT' | EQ' | GT' deriving (Eq, Show)
data Unit = Unit deriving (Eq, Show)
newtype Any' = Any' Bool deriving (Eq, Show)
newtype All' = All' Bool deriving (Eq, Show)
data Void'

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Maybe
data Maybe' a = Nothing' | Just' a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Maybe' a) where
  arbitrary = do
    a <- arbitrary
    elements [Nothing', Just' a]

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' $ f x

checkMaybe' :: SpecWith ()
checkMaybe' =
  describe "Maybe'" $ do
    it "examples" $
      fmap (+1) (Just' 1) `shouldBe` Just' 2
    it "functor identity" $
      property (functorIdentity @Maybe' @String)
    it "functor composition" $
      property (functorComposition @Maybe' @String @String @String)


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- []
data List a = Cons a (List a) | Nil deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    la <- arbitrary
    frequency [(3, return Nil), (1, return $ Cons a la)]

checkList :: SpecWith ()
checkList =
  describe "List" $ do
    it "examples" $ do
      fmap (+1) Nil `shouldBe` Nil
      fmap (+1) (Cons 1 Nil) `shouldBe` Cons 2 Nil
    it "functor identity" $
      property (functorIdentity @List @String)
    it "functor composition" $
      property (functorComposition @List @String @Int @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- NotEmpty
data List' a = End' a | Cons' a (List' a) deriving (Eq, Show)

instance Functor List' where
  fmap f (End' x)     = End' $ f x
  fmap f (Cons' x xs) = Cons' (f x) $ fmap f xs

instance Arbitrary a => Arbitrary (List' a) where
  arbitrary = do
    a <- arbitrary
    la <- arbitrary
    frequency [(3, return $ End' a), (1, return $ Cons' a la)]

checkList' :: SpecWith ()
checkList' =
  describe "List'" $ do
    it "examples" $ do
      fmap (+1) (End' 1) `shouldBe` End' 2
      fmap (+1) (Cons' 1 $ End' 2) `shouldBe` Cons' 2 (End' 3)
    it "functor identity" $
      property (functorIdentity @List' @String)
    it "functor composition" $
      property (functorComposition @List' @String @Int @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Down' a = Down' a deriving (Eq, Show)

instance Functor Down' where
  fmap f (Down' x) = Down' $ f x

instance Arbitrary a => Arbitrary (Down' a) where
  arbitrary = do
    a <- arbitrary
    return $ Down' a

checkDown' :: SpecWith ()
checkDown' =
  describe "Down'" $ do
    it "examples" $ do
      fmap (+1) (Down' 1) `shouldBe` Down' 2
    it "functor identity" $
      property (functorIdentity @Down' @String)
    it "functor composition" $
      property (functorComposition @Down' @String @Int @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Product' a = Product' a deriving (Eq, Show)

instance Functor Product' where
  fmap f (Product' x) = Product' $ f x

instance Arbitrary a => Arbitrary (Product' a) where
  arbitrary = do
    a <- arbitrary
    return $ Product' a

checkProduct' :: SpecWith ()
checkProduct' =
  describe "Product'" $ do
    it "examples" $ do
      fmap (+1) (Product' 1) `shouldBe` Product' 2
    it "functor identity" $
      property (functorIdentity @Product' @String)
    it "functor composition" $
      property (functorComposition @Product' @String @Int @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Same as Identity
newtype Last' a = Last' a deriving (Eq, Show)
newtype First' a = First' a deriving (Eq, Show)
newtype Min' a = Min' a deriving (Eq, Show)
newtype Max' a = Max' a deriving (Eq, Show)
-- Complex


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Identity' a = Identity' a deriving (Eq, Show)

instance Functor Identity' where
  fmap f (Identity' x) = Identity' $ f x

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity' a

checkIdentity' :: SpecWith ()
checkIdentity' =
  describe "Identity'" $ do
    it "examples" $ do
      fmap (+1) (Identity' 1) `shouldBe` Identity' 2
    it "functor identity" $
      property (functorIdentity @Identity' @String)
    it "functor composition" $
      property (functorComposition @Identity' @String @Int @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Same as List
newtype ZipList' a = ZipList' [a]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data ArgDesc' a
  = NoArg'                   a         -- ^   no argument expected
  | ReqArg' (String       -> a) String -- ^   required argument
  | OptArg' (Maybe String -> a) String -- ^   optional argument

instance Functor ArgDesc' where
  fmap f (NoArg' x) = NoArg' $ f x
  fmap f (ReqArg' g d) = ReqArg' (f . g) d
  fmap f (OptArg' g d) = OptArg' (f . g) d

instance Arbitrary a => Arbitrary (ArgDesc' a) where
  arbitrary = do
    a <- arbitrary
    d <- arbitrary
    elements [NoArg' a, ReqArg' (const a) d, OptArg' (const a) d]

instance Show a => Show (ArgDesc' a) where
  show (NoArg' a) = "(NoArg' " <> show a <> ")"
  show (ReqArg' _ d) = "(ReqArg' _ " <> d <> ")"
  show (OptArg' _ d) = "(OptArg' _ " <> d <> ")"

argDescFunctorIdentity :: Eq a => ArgDesc' a -> String -> Bool
argDescFunctorIdentity fa s =
  case (fa, fmap id fa) of
    (NoArg' a1, NoArg' a2) -> a1 == a2
    (ReqArg' g1 d1, ReqArg' g2 d2) -> d1 == d2 && g1 s == g2 s
    (OptArg' g1 d1, OptArg' g2 d2) -> d1 == d2 && g1 Nothing == g2 Nothing && g1 (Just s) == g2 (Just s)
    _ -> error "unexpected"

-- TODO: argDescFunctorComposition

checkArgDesc' :: SpecWith ()
checkArgDesc' =
  describe "ArgDesc'" $ do
    it "functor identity" $
      property (argDescFunctorIdentity @String)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Instead of Either
data Result' e a = Error' e | Ok' a deriving (Eq, Show)

instance Functor (Result' e) where
  fmap _ (Error' x) = Error' x
  fmap f (Ok' x) = Ok' $ f x

instance (Arbitrary e, Arbitrary a) => Arbitrary (Result' e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Error' e, Ok' a]

checkResult' :: SpecWith ()
checkResult' =
  describe "Result'" $ do
    it "examples" $ do
      fmap (+1) (Error' 1) `shouldBe` Error' 1
      fmap (+1) (Ok' 1) `shouldBe` (Ok' 2 :: (Result' () Int))
    it "functor identity" $
      property (functorIdentity @(Result' String) @Int)
    it "functor composition" $
      property (functorComposition @(Result' String) @Int @Int @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Instead of (a, b)
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

checkPair :: SpecWith ()
checkPair =
  describe "Pair" $ do
    it "examples" $
      fmap (+1) (Pair 1 2) `shouldBe` Pair 1 3
    it "functor identity" $
      property (functorIdentity @(Pair String) @Int)
    it "functor composition" $
      property (functorComposition @(Pair String) @Int @Int @Int)


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Instead of (Array i e)
newtype Array' i e = Array' [(i, e)] deriving (Eq, Show)

emptyArray' :: Array' i e
emptyArray' = Array' []

insertInArray' :: Eq i => i -> e -> Array' i e -> Array' i e
insertInArray' i e (Array' []) = Array' [(i, e)]
insertInArray' i e (Array' ((i', e') : ies))
  | i == i' = Array' $ (i, e) : ies
  | otherwise = let (Array' ies') = insertInArray' i e (Array' ies)
                in Array' ((i', e') : ies')

instance Functor (Array' i) where
  fmap _ (Array' [])             = Array' []
  fmap f (Array' ((i, e) : ies)) = let (Array' ies') = fmap f (Array' ies)
                                   in Array' ((i, f e) : ies')

instance (Arbitrary i, Arbitrary e, Eq i) => Arbitrary (Array' i e) where
  arbitrary = do
    i <- arbitrary
    e <- arbitrary
    ies <- arbitrary
    frequency [(2, return $ Array' []),
               (3, return $ Array' [(i, e)]),
               (1, return $ insertInArray' i e ies)
              ]

checkArray' :: SpecWith ()
checkArray' =
  describe "Array'" $ do
    it "examples" $ do
      emptyArray' `shouldBe` (Array' [] :: Array' () ())
      insertInArray' 1 1 emptyArray' `shouldBe` Array' [(1, 1)]
      insertInArray' 1 2 (insertInArray' 1 1 emptyArray') `shouldBe` Array' [(1, 2)]
    it "functor identity" $
      property (functorIdentity @(Array' Int) @String)
    it "functor composition" $
      property (functorComposition @(Array' Int) @String @String @String)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Right-to-left composition of functors.
newtype Compose' f g a = Compose' (f (g a)) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose' f g) where
  fmap f (Compose' fga) = Compose' $ (fmap . fmap) f fga

composeFunctorIdentity :: (Eq a) => a -> Bool
composeFunctorIdentity a = fmap id fa == fa
  where fa = Compose' (Just [a])

composeFunctorComposition :: Eq c => Fun b c -> Fun a b -> a -> Bool
composeFunctorComposition (Fn f) (Fn g) a = fmap (f . g) fa == (fmap f . fmap g) fa
  where fa = Compose' (Just [a])
composeFunctorComposition _ _ _ = error "Make the compiler happy"

checkCompose' :: SpecWith ()
checkCompose' =
  describe "Compose'" $ do
    it "functor identity" $
      property (composeFunctorIdentity @String)
    it "functor identity" $
      property (composeFunctorComposition @String @String @String)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Instead of (a -> b)
newtype Fn' a b = Fn' (a -> b)

instance Functor (Fn' a) where
  fmap f (Fn' f') = Fn' $ f . f'

instance Arbitrary b => Arbitrary (Fn' a b) where
  arbitrary = do
    b <- arbitrary
    return $ Fn' $ const b

fnFunctorIdentity :: Eq b => Blind (Fn' a b) -> a -> Bool
fnFunctorIdentity (Blind fab) a = let (Fn' f1) = fab
                                      (Fn' f2) = fmap id fab
                                  in f1 a == f2 a

fnFunctorComposition :: Eq d => Fun c d -> Fun b c -> Blind (Fn' a b) -> a -> Bool
fnFunctorComposition (Fn f) (Fn g) (Blind fab) a = let (Fn' f1) = fmap (f . g) fab
                                                       (Fn' f2) = (fmap f . fmap g) fab
                                                   in f1 a == f2 a
fnFunctorComposition _ _ _ _ = error "Make the compiler happy"


checkFn' :: SpecWith ()
checkFn' =
  describe "Fn'" $ do
    it "functor identity" $
      property (fnFunctorIdentity @Int @String)
    it "functor composition" $
      property (fnFunctorComposition @Int @Int @String @String)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Map' k v = Map' [(k, v)] deriving (Eq, Show)

emptyMap' :: Map' k v
emptyMap' = Map' []

keysOfMap' :: Map' k v -> [k]
keysOfMap' (Map' kvs) = map fst kvs

insertInMap' :: Eq k => (k, v) -> Map' k v -> Map' k v
insertInMap' kv (Map' []) = Map' [kv]
insertInMap' (k1, v1) m@(Map' ((k2, v2) : kvs))
  | k1 == k2  = m
  | otherwise = let (Map' kvs') = insertInMap' (k1, v1) (Map' kvs)
                in Map' ((k2, v2) : kvs')

instance Eq k => Semigroup (Map' k v) where
  (Map' []) <> m = m
  m <> (Map' []) = m
  m <> (Map' (kv : kvs)) = insertInMap' kv m <> Map' kvs

instance Eq k => Monoid (Map' k v) where
  mempty = Map' []

instance Functor (Map' k) where
  fmap _ (Map' [])             = Map' []
  fmap f (Map' ((k, v) : kvs)) = let (Map' kvs') = fmap f (Map' kvs)
                                 in Map' ((k, f v) : kvs')

instance (Eq k, Arbitrary k, Arbitrary v) => Arbitrary (Map' k v) where
  arbitrary = do
    kvs <- listOf arbitrary
    return $ foldr insertInMap' emptyMap' kvs

checkMap' :: SpecWith ()
checkMap' =
  describe "Map'" $ do
    it "examples" $ do
      emptyMap' `shouldBe` (Map' [] :: Map' () ())
      insertInMap' (1, 1) emptyMap' `shouldBe` Map' [(1, 1)]
      insertInMap' (1, 2) (insertInMap' (1, 1) emptyMap') `shouldBe` Map' [(1, 1)]
    it "functor identity" $
      property (functorIdentity @(Map' Int) @String)
    it "functor composition" $
      property (functorComposition @(Map' Int) @String @String @String)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Set' v = Set' (Map' v ()) deriving (Eq)

instance Eq v => Semigroup (Set' v) where
  (Set' s1) <> (Set' s2) = Set' (s1 <> s2)

instance Eq v => Monoid (Set' v) where
  mempty = Set' mempty

-- instance Eq b => Functor Set' where
--   -- fmap :: forall a b. (a -> b) -> Set' a -> Set' b
--   fmap f (Set' m) = setOf $ fmap f (keysOfMap' m)

instance (Eq v, Arbitrary v) => Arbitrary (Set' v) where
  arbitrary = do
    m <- arbitrary
    return $ Set' m

instance Show v => Show (Set' v) where
  show (Set' m) = "Set' (" ++ keys ++ ")"
    where keys = intercalate ", " (map show (keysOfMap' m))


setOf :: Eq v => [v] -> Set' v
setOf vs = Set' $ foldr insertInMap' mempty kvs
  where kvs = map (flip (,) ()) (reverse vs)

-- checkSet' :: SpecWith ()
-- checkSet' =
--   describe "Set'" $ do
--     it "functor identity" $
--       property (functorIdentity @(Set' Int) @String)
--     it "functor composition" $
--       property (functorComposition @(Set' Int) @String @String @String)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity fa = fmap id fa == fa

functorComposition :: (Functor f, Eq (f c)) => Fun b c -> Fun a b -> f a -> Bool
functorComposition (Fn f) (Fn g) fa = fmap (f . g) fa == (fmap f . fmap g) fa
functorComposition _ _ _ = error "Make the compiler happy"

check :: IO ()
check = do
  hspec $ do
    checkMaybe'
    checkList
    checkList'
    checkDown'
    checkProduct'
    checkIdentity'
    checkArgDesc'
    checkResult'
    checkPair
    checkArray'
    checkCompose'
    checkFn'
    checkMap'
    -- checkSet'
