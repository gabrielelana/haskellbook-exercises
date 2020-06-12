{-# LANGUAGE TypeApplications #-}

module Meetup.SemigroupsAndMonoids where

import Data.Semigroup
import Test.QuickCheck
import Test.Hspec
import Data.Ord

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Ordering' = LT' | EQ' | GT' deriving (Eq, Show)

instance Semigroup Ordering' where
  LT' <> _ = LT'
  EQ' <> o = o
  GT' <> _ = GT'

instance Monoid Ordering' where
  mempty = EQ'

instance Arbitrary Ordering' where
  arbitrary = elements [LT', EQ', GT']

checkOrdering' :: SpecWith ()
checkOrdering' =
  describe "Ordering'" $ do
    it "examples" $ do
      LT' <> EQ' `shouldBe` LT'
      GT' <> EQ' `shouldBe` GT'
      LT' <> GT' `shouldBe` LT'
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @Ordering')
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @Ordering')
    it "monoid right identity" $
      property (monoidRightIdentityProperty @Ordering')

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Unit = Unit deriving (Eq, Show)

instance Semigroup Unit where
  Unit <> Unit = Unit

instance Monoid Unit where
  mempty = Unit

instance Arbitrary Unit where
  arbitrary = return Unit

checkUnit :: SpecWith ()
checkUnit =
  describe "Unit" $ do
    it "examples" $ do
      Unit <> Unit `shouldBe` Unit
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @Unit)
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @Unit)
    it "monoid right identity" $
      property (monoidRightIdentityProperty @Unit)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Boolean disjunction (||)
newtype Any' = Any' Bool deriving (Eq, Show)

instance Semigroup Any' where
  (Any' False) <> (Any' False) = Any' False
  _            <> _            = Any' True

instance Monoid Any' where
  mempty = Any' False

instance Arbitrary Any' where
  arbitrary = elements [Any' True, Any' False]

checkAny' :: SpecWith ()
checkAny' =
  describe "Any'" $ do
    it "examples" $ do
      Any' True <> Any' True `shouldBe` Any' True
      Any' False <> Any' True `shouldBe` Any' True
      Any' False <> Any' False `shouldBe` Any' False
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @Any')
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @Any')
    it "monoid right identity" $
      property (monoidRightIdentityProperty @Any')

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Boolean conjunction (&&)
newtype All' = All' Bool deriving (Eq, Show)

instance Semigroup All' where
  (All' True) <> (All' True) = All' True
  _           <> _           = All' False

instance Monoid All' where
  mempty = All' True

instance Arbitrary All' where
  arbitrary = elements [All' True, All' False]

checkAll' :: SpecWith ()
checkAll' = do
  describe "All'" $ do
    it "examples" $ do
      (All' True) <> (All' True) `shouldBe` (All' True)
      (All' True) <> (All' False) `shouldBe` (All' False)
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @All')
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @All')
    it "monoid right identity" $
      property (monoidRightIdentityProperty @All')

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Void'

instance Semigroup Void' where
  a <> _ = a

-- Cannot have an instance of Monoid

-- Don't know how to test it :-/

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data List a = Cons a (List a) | Nil deriving (Eq, Show)

instance Semigroup (List a) where
  xs <> Nil = xs
  Nil <> xs = xs
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
  mempty = Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    elements [Nil, Cons a l]

checkList :: SpecWith ()
checkList = do
  describe "List" $ do
    it "examples" $ do
      (Cons 1 (Cons 2 Nil)) <> (Cons 3 Nil) `shouldBe` (Cons 1 (Cons 2 (Cons 3 Nil)))
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(List String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(List String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(List String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data Maybe' a = Nothing' | Just' a deriving (Eq, Show)

instance Semigroup a => Semigroup (Maybe' a) where
  Nothing' <> a = a
  a <> Nothing' = a
  (Just' al) <> (Just' ar) = Just' (al <> ar)

instance Semigroup a => Monoid (Maybe' a) where
  mempty = Nothing'

instance Arbitrary a => Arbitrary (Maybe' a) where
  arbitrary = do
    a <- arbitrary
    elements [Nothing', Just' a]

checkMaybe' :: SpecWith ()
checkMaybe' = do
  describe "Maybe'" $ do
    it "examples" $ do
      (Just' (Sum 1)) <> (Just' (Sum 2)) `shouldBe` (Just' (Sum 3))
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Maybe' String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Maybe' String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Maybe' String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- TODO: IO

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data List' a = End' a | Cons' a (List' a) deriving (Eq, Show)

instance Semigroup (List' a) where
  (End' x) <> ys = Cons' x ys
  (Cons' x xs) <> ys = Cons' x (xs <> ys)

-- There's no instance of semigroup because there's no way to have `mempty`

instance Arbitrary a => Arbitrary (List' a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    elements [End' a, Cons' a l]

checkList' :: SpecWith ()
checkList' = do
  describe "List'" $ do
    it "examples" $ do
      (End' 1) <> (End' 2) `shouldBe` (Cons' 1 (End' 2))
      (Cons' 1 (End' 2)) <> (End' 3) `shouldBe` (Cons' 1 (Cons' 2 (End' 3)))
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(List' String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Down' a = Down' a deriving (Eq, Show)

instance Semigroup a => Semigroup (Down' a) where
  (Down' al) <> (Down' ar) = Down' (al <> ar)

instance Monoid a => Monoid (Down' a) where
  mempty = Down' mempty

instance Arbitrary a => Arbitrary (Down' a) where
  arbitrary = do
    a <- arbitrary
    return $ Down' a

checkDown' :: SpecWith ()
checkDown' = do
  describe "Down'" $ do
    it "examples" $ do
      (Down' "Meet") <> (Down' "up") `shouldBe` (Down' "Meetup")
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Down' String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Down' String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Down' String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Product' a = Product' a deriving (Eq, Show)

-- Type constraints are not put on the data declaration but on the
-- instance declaration

instance Num a => Semigroup (Product' a) where
  (Product' x) <> (Product' y) = Product' (x * y)

instance Num a => Monoid (Product' a) where
  mempty = Product' 1

instance (Num a, Arbitrary a) => Arbitrary (Product' a) where
  arbitrary = do
    a <- arbitrary
    return $ Product' a

checkProduct' :: SpecWith ()
checkProduct' = do
  describe "Product'" $ do
    it "examples" $ do
      (Product' 3) <> (Product' 2) `shouldBe` (Product' 6)
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Product' Int))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Product' Int))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Product' Int))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Sum' a = Sum' a deriving (Eq, Show)

instance Num a => Semigroup (Sum' a) where
  (Sum' x) <> (Sum' y) = Sum' (x + y)

instance Num a => Monoid (Sum' a) where
  mempty = Sum' 0

instance (Num a, Arbitrary a) => Arbitrary (Sum' a) where
  arbitrary = do
    a <- arbitrary
    return $ Sum' a

checkSum' :: SpecWith ()
checkSum' = do
  describe "Sum'" $ do
    it "examples" $ do
      (Sum' 3) <> (Sum' 2) `shouldBe` (Sum' 5)
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Sum' Int))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Sum' Int))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Sum' Int))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Endo' a = Endo' {appEndo' :: (a -> a)}

instance Semigroup (Endo' a) where
  (Endo' fl) <> (Endo' fr) = Endo' (fr . fl)

instance Monoid (Endo' a) where
  mempty = Endo' id

endoSemigroupAP :: Eq a
                => Blind (a -> a)
                -> Blind (a -> a)
                -> Blind (a -> a)
                -> a
                -> Bool
endoSemigroupAP (Blind f) (Blind g) (Blind h) a =
  appEndo' (f' <> (g' <> h')) a == appEndo' ((f' <> g') <> h') a
  where f' = Endo' f
        g' = Endo' g
        h' = Endo' h

endoMonoidLIP :: Eq a => Blind (a -> a) -> a -> Bool
endoMonoidLIP (Blind f) a = appEndo' (mempty <> (Endo' f)) a == f a

endoMonoidRIP :: Eq a => Blind (a -> a) -> a -> Bool
endoMonoidRIP (Blind f) a = appEndo' ((Endo' f) <> mempty) a == f a

checkEndo' :: SpecWith ()
checkEndo' = do
  describe "Endo'" $ do
    it "examples" $ do
      let computation = Endo' ("Hello, " ++) <> Endo' (++ "!")
      appEndo' computation "Haskell" `shouldBe` "Hello, Haskell!"
    it "semigroup associativity" $
      property (endoSemigroupAP @Int)
    it "monoid left identity" $
      property (endoMonoidLIP @Int)
    it "monoid right identity" $
      property (endoMonoidRIP @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Dual' a = Dual' {getDual' :: a} deriving (Eq, Show)

instance Semigroup a => Semigroup (Dual' a) where
  (Dual' x) <> (Dual' y) = Dual' (y <> x)

instance Monoid a => Monoid (Dual' a) where
  mempty = Dual' mempty

instance Arbitrary a => Arbitrary (Dual' a) where
  arbitrary = do
    a <- arbitrary
    return $ Dual' a

checkDual' :: SpecWith ()
checkDual' = do
  describe "Dual'" $ do
    it "examples" $ do
      getDual' (mappend (Dual' "Hello") (Dual' "World")) `shouldBe` "WorldHello"
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Dual' String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Dual' String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Dual' String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Last' a = Last' (Maybe a) deriving (Eq, Show)

instance Semigroup (Last' a) where
  x <> (Last' Nothing) = x
  _ <> y               = y

instance Monoid (Last' a) where
  mempty = Last' Nothing

instance Arbitrary a => Arbitrary (Last' a) where
  arbitrary = do
    a <- arbitrary
    elements [Last' Nothing, Last' (Just a)]

checkLast' :: SpecWith ()
checkLast' = do
  describe "Last'" $ do
    it "examples" $ do
      (Last' (Just 1)) <> (Last' Nothing) `shouldBe` Last' (Just 1)
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Last' String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Last' String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Last' String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype First' a = First' (Maybe a) deriving (Eq, Show)

instance Semigroup (First' a) where
  (First' Nothing) <> y = y
  x <> _                = x

instance Monoid (First' a) where
  mempty = First' Nothing

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    elements [First' Nothing, First' (Just a)]

checkFirst' :: SpecWith ()
checkFirst' = do
  describe "First'" $ do
    it "examples" $ do
      (First' (Just 1)) <> (First' Nothing) `shouldBe` First' (Just 1)
      (First' Nothing) <> (First' (Just 1)) `shouldBe` First' (Just 1)
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(First' String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(First' String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(First' String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Identity' a = Identity' a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity' a) where
  (Identity' x) <> (Identity' y) = Identity' (x <> y)

instance Monoid a => Monoid (Identity' a) where
  mempty = Identity' mempty

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity' a

checkIdentity' :: SpecWith ()
checkIdentity' = do
  describe "Identity'" $ do
    it "examples" $ do
      (Identity' (Sum 1)) <> (Identity' (Sum 2)) `shouldBe` Identity' (Sum 3)
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Identity' String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Identity' String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Identity' String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Max' a = Max' a deriving (Eq, Show)

instance Ord a => Semigroup (Max' a) where
  (Max' x) <> (Max' y) = Max' $ case compare x y of
                                  LT -> y
                                  EQ -> x
                                  GT -> x

-- Look at the use of Bounded
instance (Ord a, Bounded a) => Monoid (Max' a) where
  mempty = Max' minBound

-- Using all the constraints to avoid generation of values that don't
-- have instances
instance (Arbitrary a, Ord a, Bounded a) => Arbitrary (Max' a) where
  arbitrary = do
    a <- arbitrary
    return $ Max' a

checkMax' :: SpecWith ()
checkMax' = do
  describe "Max'" $ do
    it "examples" $ do
      (Max' 10) <> (Max' 20) `shouldBe` (Max' 20)
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Max' Int))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Max' Int))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Max' Int))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Min' a = Min' a deriving (Eq, Show)

instance Ord a => Semigroup (Min' a) where
  (Min' x) <> (Min' y) = Min' $ case compare x y of
                                  LT -> x
                                  EQ -> x
                                  GT -> y

-- Look at the use of Bounded
instance (Ord a, Bounded a) => Monoid (Min' a) where
  mempty = Min' maxBound

-- Using all the constraints to avoid generation of values that don't
-- have instances
instance (Arbitrary a, Ord a, Bounded a) => Arbitrary (Min' a) where
  arbitrary = do
    a <- arbitrary
    return $ Min' a

checkMin' :: SpecWith ()
checkMin' = do
  describe "Min'" $ do
    it "examples" $ do
      (Min' 10) <> (Min' 20) `shouldBe` (Min' 10)
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Min' Int))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Min' Int))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Min' Int))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- This data type represents an equivalence relation (reflexive,
-- symmetrical, transitive)
newtype Equivalence' a = Equivalence' { getEquivalence' :: a -> a -> Bool }

instance Semigroup (Equivalence' a) where
  (Equivalence' f) <> (Equivalence' g) = Equivalence' h
    where h x y = f x y && g x y

instance Monoid (Equivalence' a) where
  mempty = Equivalence' $ (const . const) True

-- f <> (g <> h) == (f <> g) <> h
equivalenceSemigroupAP :: (Blind (a -> a -> Bool))
                       -> (Blind (a -> a -> Bool))
                       -> (Blind (a -> a -> Bool))
                       -> a
                       -> a
                       -> Bool
equivalenceSemigroupAP (Blind f1) (Blind f2) (Blind f3) a1 a2 =
  getEquivalence' (f1' <> (f2' <> f3')) a1 a2 ==
  getEquivalence' ((f1' <> f2') <> f3') a1 a2
  where f1' = Equivalence' f1
        f2' = Equivalence' f2
        f3' = Equivalence' f3

-- mempty <> f == f
equivalenceMonoidLIP :: (Blind (a -> a -> Bool)) -> a -> a -> Bool
equivalenceMonoidLIP (Blind f) a1 a2 =
  getEquivalence' (mempty <> (Equivalence' f)) a1 a2 == f a1 a2

-- f <> mempty == f
equivalenceMonoidRIP :: (Blind (a -> a -> Bool)) -> a -> a -> Bool
equivalenceMonoidRIP (Blind f) a1 a2 =
  getEquivalence' ((Equivalence' f) <> mempty) a1 a2 == f a1 a2

-- The semigroup operation (<>) must be closed, aka must give you back
-- an element of the same set of the operands. Therefore (x <> y) must
-- be an equivalence relation

-- f a a == True
equivalenceReflexiveProperty :: (a -> a -> Bool) -> a -> Bool
equivalenceReflexiveProperty f a = (f a a) == True

-- f a b == f b a
equivalenceSymmetryProperty :: (a -> a -> Bool) -> a -> a -> Bool
equivalenceSymmetryProperty f a b = f a b == f b a

-- if f a b && f b c then f a c
equivalenceTransitivityProperty :: (a -> a -> Bool) -> a -> a -> a -> Bool
equivalenceTransitivityProperty f a b c = if (f a b) && (f b c) then (f a c) else True

equivalenceRelationProperty :: (Equivalence' a) -> a -> a -> a -> Bool
equivalenceRelationProperty (Equivalence' f) a b c =
  (equivalenceReflexiveProperty f a)
  && equivalenceSymmetryProperty f a b
  && equivalenceTransitivityProperty f a b c

checkEquivalence' :: SpecWith ()
checkEquivalence' = do
  describe "Equivalence'" $ do
    let parity = Equivalence' $ \x y -> (odd x) == (odd y)
    let multipleOf n = Equivalence' $ \x y -> (mod x n) == (mod y n)
    let multipleOf5 = multipleOf 5
    it "examples" $ do
      getEquivalence' parity 5 7 `shouldBe` True
      getEquivalence' parity 4 6 `shouldBe` True
      getEquivalence' parity 4 5 `shouldBe` False
      getEquivalence' multipleOf5 5 15 `shouldBe` True
      getEquivalence' (parity <> multipleOf5) 5 15 `shouldBe` True
      getEquivalence' (parity <> multipleOf5) 5 17 `shouldBe` False
    it "semigroup associativity" $
      property (equivalenceSemigroupAP @Int)
    it "monoid left identity" $
      property (equivalenceMonoidLIP @Int)
    it "monoid right identity" $
      property (equivalenceMonoidRIP @Int)
    it "parity is an equivalence relation property" $
      property (equivalenceRelationProperty @Int parity)
    it "multipleOf is an equivalence relation property" $
      forAll (suchThat (arbitrary @Int) ((/=) 0)) $ \n ->
        equivalenceRelationProperty @Int (multipleOf n)
    let parityGenerator = pure parity
    let multipleOfGenerator = (suchThat (arbitrary @Int) ((/=) 0)) >>= (pure . multipleOf)
    let equivalenceRelationship = oneof [parityGenerator, multipleOfGenerator]
    it "composition of equivalence relations is an equivalence relation" $
      forAllBlind equivalenceRelationship $ \f ->
        forAllBlind equivalenceRelationship $ \g ->
          equivalenceRelationProperty @Int (f <> g)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Defines a total ordering on a type as per compare
newtype Comparison' a = Comparison' { getComparison' :: a -> a -> Ordering }

-- The official implementation is
-- `deriving instance Semigroup (Comparison a)`
instance Ord a => Semigroup (Comparison' a) where
  (Comparison' f) <> (Comparison' g) = Comparison' $ f <> g

instance Ord a => Monoid (Comparison' a) where
  mempty = Comparison' $ (const . const) (mempty @Ordering)

-- (f <> (g <> h)) == ((f <> g) <> h)
comparisonSemigroupAP :: Ord a
                      => Blind (a -> a -> Ordering)
                      -> Blind (a -> a -> Ordering)
                      -> Blind (a -> a -> Ordering)
                      -> a
                      -> a
                      -> Bool
comparisonSemigroupAP (Blind f) (Blind g) (Blind h) a1 a2 =
  getComparison' (f' <> (g' <> h')) a1 a2
  ==
  getComparison' ((f' <> g') <> h') a1 a2
  where f' = Comparison' f
        g' = Comparison' g
        h' = Comparison' h

-- mempty <> f == f
comparisonMonoidLIP :: Ord a
                    => Blind (a -> a -> Ordering)
                    -> a
                    -> a
                    -> Bool
comparisonMonoidLIP (Blind f) a1 a2 =
  getComparison' (mempty <> (Comparison' f)) a1 a2 == f a1 a2

-- f <> mempty == f
comparisonMonoidRIP :: Ord a
                    => Blind (a -> a -> Ordering)
                    -> a
                    -> a
                    -> Bool
comparisonMonoidRIP (Blind f) a1 a2 =
  getComparison' ((Comparison' f) <> mempty) a1 a2 == f a1 a2

checkComparison' :: SpecWith ()
checkComparison' = do
  describe "Comparison'" $ do
    it "semigroup associativity" $
      property (comparisonSemigroupAP @Int)
    it "monoid left identity" $
      property (comparisonMonoidLIP @Int)
    it "monoid right identity" $
      property (comparisonMonoidRIP @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Predicate' a = Predicate' { getPredicate' :: a -> Bool }

-- I guess that the composition of predicates it's true when the two
-- predicates are true
instance Semigroup (Predicate' a) where
  (Predicate' f) <> (Predicate' g) = Predicate' $ \x -> f x && g x

instance Monoid (Predicate' a) where
  mempty = Predicate' $ const True

-- (f <> (g <> h)) == ((f <> g) <> h)
predicateSemigroupAP :: Ord a
                      => Blind (a -> Bool)
                      -> Blind (a -> Bool)
                      -> Blind (a -> Bool)
                      -> a
                      -> Bool
predicateSemigroupAP (Blind f) (Blind g) (Blind h) a =
  getPredicate' (f' <> (g' <> h')) a
  ==
  getPredicate' ((f' <> g') <> h') a
  where f' = Predicate' f
        g' = Predicate' g
        h' = Predicate' h

-- mempty <> f == f
predicateMonoidLIP :: Ord a
                    => Blind (a -> Bool)
                    -> a
                    -> Bool
predicateMonoidLIP (Blind f) a =
  getPredicate' (mempty <> (Predicate' f)) a == f a

-- f <> mempty == f
predicateMonoidRIP :: Ord a
                    => Blind (a -> Bool)
                    -> a
                    -> Bool
predicateMonoidRIP (Blind f) a =
  getPredicate' ((Predicate' f) <> mempty) a == f a

checkPredicate' :: SpecWith ()
checkPredicate' = do
  describe "Predicate'" $ do
    it "examples" $ do
      let greaterThan5 = Predicate' (> 5)
      let lessThan10 = Predicate' (< 10)
      getPredicate' (greaterThan5 <> lessThan10) 6 `shouldBe` True
      getPredicate' (greaterThan5 <> lessThan10) 11 `shouldBe` False
    it "semigroup associativity" $
      property (predicateSemigroupAP @Int)
    it "monoid left identity" $
      property (predicateMonoidLIP @Int)
    it "monoid right identity" $
      property (predicateMonoidRIP @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Instead of (a -> b)
newtype Join' a b = Join' { getJoin' :: a -> b }

instance Semigroup b => Semigroup (Join' a b) where
  (Join' f) <> (Join' g) = Join' $ \x -> (f x) <> (g x)

instance Monoid b => Monoid (Join' a b) where
  mempty = Join' $ const mempty

-- (f <> (g <> h)) == ((f <> g) <> h)
joinSemigroupAP :: (Semigroup b, Eq b)
                => Blind (a -> b)
                -> Blind (a -> b)
                -> Blind (a -> b)
                -> a
                -> Bool
joinSemigroupAP (Blind f) (Blind g) (Blind h) a =
  getJoin' (f' <> (g' <> h')) a
  ==
  getJoin' ((f' <> g') <> h') a
  where f' = Join' f
        g' = Join' g
        h' = Join' h

-- mempty <> f == f
joinMonoidLIP :: (Monoid b, Eq b)
              => Blind (a -> b)
              -> a
              -> Bool
joinMonoidLIP (Blind f) a =
  getJoin' (mempty <> (Join' f)) a == f a

-- f <> mempty == f
joinMonoidRIP :: (Monoid b, Eq b)
              => Blind (a -> b)
              -> a
              -> Bool
joinMonoidRIP (Blind f) a =
  getJoin' ((Join' f) <> mempty) a == f a

checkJoin' :: SpecWith ()
checkJoin' = do
  describe "Join'" $ do
    it "examples" $ do
      let f1 = Join' (const (Sum 1))
      let f2 = Join' (const (Sum 2))
      getJoin' (f1 <> f2) () `shouldBe` (Sum 3)
    it "semigroup associativity" $
      property (joinSemigroupAP @String @Int)
    it "monoid left identity" $
      property (joinMonoidLIP @String @Int)
    it "monoid right identity" $
      property (joinMonoidRIP @String @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Instead of Either
data Result' e a = Error' e | Ok' a deriving (Eq, Show)

instance Semigroup (Result' e a) where
  (Error' _) <> x = x
  x          <> _ = x

-- Doesn't have an instance of monoid, discuss why

instance (Arbitrary e, Arbitrary a) => Arbitrary (Result' e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    elements [Error' e, Ok' a]

checkResult' :: SpecWith ()
checkResult' = do
  describe "Result'" $ do
    it "examples" $ do
      (Ok' 1) <> (Ok' 2) `shouldBe` ((Ok' 1) :: Result' String Int)
      (Ok' 1) <> (Error' "???") `shouldBe` ((Ok' 1) :: Result' String Int)
      (Error' "???") <> (Ok' 1) `shouldBe` ((Ok' 1) :: Result' String Int)
      (Error' "???") <> (Error' "!!!") `shouldBe` ((Error' "!!!") :: Result' String Int)
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Result' String Int))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Instead of (a, b)
data Pair a b = Pair a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (Pair a1 b1) <> (Pair a2 b2) = Pair (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

checkPair :: SpecWith ()
checkPair = do
  describe "Pair" $ do
    it "examples" $ do
      (Pair (Sum 1) "a") <> (Pair (Sum 2) "b") `shouldBe` Pair (Sum 3) "ab"
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Pair (Sum Int) String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Pair (Sum Int) String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Pair (Sum Int) String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Proxy is a type that holds no data, but has a phantom parameter of
-- arbitrary type
data Proxy' a = Proxy' deriving (Eq, Show)

instance Semigroup (Proxy' a) where
  _ <> _ = Proxy'

instance Monoid (Proxy' a) where
  mempty = Proxy'

instance Arbitrary (Proxy' a) where
  arbitrary = return Proxy'

checkProxy' :: SpecWith ()
checkProxy' = do
  describe "Proxy'" $ do
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Proxy' String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Proxy' String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Proxy' String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Dual function arrows
newtype Op' a b = Op' { getOp' :: b -> a }

instance Semigroup a => Semigroup (Op' a b) where
  (Op' f) <> (Op' g) = Op' $ \x -> (f x) <> (g x)

instance Monoid a => Monoid (Op' a b) where
  mempty = Op' $ const mempty

-- (f <> (g <> h)) == ((f <> g) <> h)
opSemigroupAP :: (Semigroup a, Eq a)
                => Blind (b -> a)
                -> Blind (b -> a)
                -> Blind (b -> a)
                -> b
                -> Bool
opSemigroupAP (Blind f) (Blind g) (Blind h) x =
  getOp' (f' <> (g' <> h')) x
  ==
  getOp' ((f' <> g') <> h') x
  where f' = Op' f
        g' = Op' g
        h' = Op' h

-- mempty <> f == f
opMonoidLIP :: (Monoid a, Eq a)
              => Blind (b -> a)
              -> b
              -> Bool
opMonoidLIP (Blind f) x =
  getOp' (mempty <> (Op' f)) x == f x

-- f <> mempty == f
opMonoidRIP :: (Monoid a, Eq a)
              => Blind (b -> a)
              -> b
              -> Bool
opMonoidRIP (Blind f) x =
  getOp' ((Op' f) <> mempty) x == f x

checkOp' :: SpecWith ()
checkOp' = do
  describe "Op'" $ do
    it "semigroup associativity" $
      property (opSemigroupAP @String @Int)
    it "monoid left identity" $
      property (opMonoidLIP @String @Int)
    it "monoid right identity" $
      property (opMonoidRIP @String @Int)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newtype Const' a b = Const' { getConst' :: a } deriving (Eq, Show)

instance Semigroup a => Semigroup (Const' a b) where
  (Const' x) <> (Const' y) = Const' $ x <> y

instance Monoid a => Monoid (Const' a b) where
  mempty = Const' mempty

instance Arbitrary a => Arbitrary (Const' a b) where
  arbitrary = do
    a <- arbitrary
    return $ Const' a

checkConst' :: SpecWith ()
checkConst' = do
  describe "Const'" $ do
    it "semigroup associativity" $
      property (semigroupAssociativityProperty @(Const' String String))
    it "monoid left identity" $
      property (monoidLeftIdentityProperty @(Const' String String))
    it "monoid right identity" $
      property (monoidRightIdentityProperty @(Const' String String))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

semigroupAssociativityProperty :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssociativityProperty x y z = x <> (y <> z) == (x <> y) <> z

monoidLeftIdentityProperty :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentityProperty x = mempty <> x == x

monoidRightIdentityProperty :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentityProperty x = x <> mempty == x

check :: IO ()
check = do
  hspec $ do
    checkOrdering'
    checkUnit
    checkAny'
    checkAll'
    checkList
    checkMaybe'
    checkList'
    checkDown'
    checkProduct'
    checkSum'
    checkEndo'
    checkDual'
    checkLast'
    checkFirst'
    checkIdentity'
    checkMax'
    checkMin'
    checkEquivalence'
    checkComparison'
    checkPredicate'
    checkJoin'
    checkResult'
    checkPair
    checkProxy'
    checkOp'
    checkConst'
