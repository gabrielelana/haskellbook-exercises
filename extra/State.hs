{-# LANGUAGE TupleSections #-}

module State where

import Data.Bool
import Control.Monad
import System.Random
import Data.Functor ((<&>))
import Control.Applicative

-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State

-- Model a turnstile
-- coin :: Locked -> (Thanks, Unlocked)
-- coin :: Unlocked -> (Thanks, Unlocked)
-- push :: Locked -> (Tut, Locked)
-- push :: Unlocked -> (Open, Locked)

data TState = Locked | Unlocked deriving (Eq, Show)
data TOutput = Thanks | Tut | Open deriving (Eq, Show)

coin :: TState -> (TOutput, TState)
coin _ = (Thanks, Unlocked)

push :: TState -> (TOutput, TState)
push Unlocked = (Open, Locked)
push Locked = (Tut, Locked)

-- What happens on Monday? How can we chain things together?

monday :: TState -> ([TOutput], TState)
monday s0 = let (a0, s1) = coin s0
                (a1, s2) = push s1
                (a2, s3) = push s2
                (a3, s4) = coin s3
                (a4, s5) = push s4
            in ([a0, a1, a2, a3, a4], s5)

-- monday Locked == ([Thanks,Open,Tut,Thanks,Open],Locked)

-- X: Implement function `regularPerson` always inserts a coin then
-- pushes the arm
regularPerson :: TState -> ([TOutput], TState)
regularPerson s0 = let (a0, s1) = coin s0
                       (a1, s2) = push s1
                   in ([a0, a1], s2)

-- X: Implement function `distractedPerson` always inserts a coin then
-- wanders off without going through
distractedPerson :: TState -> ([TOutput], TState)
distractedPerson s0 = let (a0, s1) = coin s0
                      in ([a0], s1)

-- X: Implement function `hastyPerson` first pushes the arm without
-- having inserted a coin. If it opens (for example, they're following
-- a `distractedPerson`) they go through. If not (for example they're
-- following a `regularPerson`) they'll then insert a coin and push
-- the arm to go through.
hastyPerson :: TState -> ([TOutput], TState)
hastyPerson s0 = case push s0 of
                   (Thanks, _) -> error "unreachable"
                   (Open, s1) -> ([Open], s1)
                   (Tut, s1) -> let (a1, s2) = coin s1
                                in ([Tut, a1], s2)

-- X: Use  the  functions  above  to implement  `tuesday`  returning  the
-- outputs from this sequence of visitors: regularPerson, hastyPerson,
-- distractedPerson, hastyPerson

tuesday :: TState -> ([TOutput], TState)
tuesday s0 = let (a0, s1) = regularPerson s0
                 (a1, s2) = hastyPerson s1
                 (a2, s3) = distractedPerson s2
                 (a3, s4) = hastyPerson s3
             in (a0 <> a1 <> a2 <> a3, s4)

-- X: Implement luckyPair :: Bool -> TurnstileState -> (Bool,
-- TurnstileState) representing two people attempting to use the
-- turnstile in succession. The first is either a regularPerson or a
-- distractedPerson (depending on the Bool argument). The second
-- person will simply push the arm without inserting a coin and give
-- up if they don't get through. The Bool result should indicate
-- whether the second person made it through.

luckyPair :: Bool -> TState -> (Bool, TState)
luckyPair b s0 = let (_, s1) = bool regularPerson distractedPerson b s0
                     (a1, s2) = push s1
                 in (a1 == Open, s2)

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State


-- X: Implement instance of Functor

instance Functor (State s) where
  -- (a -> b) -> f a -> f b
  -- (a -> b) -> (State (s -> (a, s))) -> (State (s -> (b, s)))
  fmap f (State fa) = State $ \s -> let (a, s') = fa s in (f a, s')

-- X: Implement instance of Applicative

instance Applicative (State s) where
  pure a = State $ (,) a

  -- f (a -> b) -> f a -> f b
  -- (State (s -> ((a -> b), s))) -> (State (s -> (a, s))) -> (State (s -> (b, s)))
  (State ff) <*> (State fa) = State $ \s -> let (fab, s') = ff s
                                                (a, s'') = fa s'
                                            in (fab a, s'')

-- X: Implement instance of Monad

instance Monad (State s) where
  return = pure

  -- m a -> (a -> m b) -> m b
  -- (State (s -> a, s)) -> (a -> (State (s -> (b, s)))) -> (State (s -> (b, s)))
  fa >>= ff = State $ \s -> let (a, s') = runState fa s in runState (ff a) s'

-- Review coin and push with State
coinS :: State TState TOutput
coinS = State $ const (Thanks, Unlocked)

pushS :: State TState TOutput
pushS = State f
  where f Unlocked = (Open, Locked)
        f Locked = (Tut, Locked)

mondayS :: State TState [TOutput]
mondayS = do
  a0 <- coinS
  a1 <- pushS
  a2 <- pushS
  a3 <- coinS
  a4 <- pushS
  return [a0, a1, a2, a3, a4]

-- runState mondayS Locked == ([Thanks,Open,Tut,Thanks,Open],Locked)
-- runState mondayS Locked == monday Locked

mondayS' :: State TState [TOutput]
mondayS' =
  coinS >>= \a0 ->
    pushS >>= \a1 ->
      pushS >>= \a2 ->
        coinS >>= \a3 ->
          pushS >>= \a4 ->
            return [a0, a1, a2, a3, a4]

mondayS'' :: State TState [TOutput]
mondayS'' = sequence [coinS, pushS, pushS, coinS, pushS]

-- X: Implement the functions `regularPersonS`, `distractedPersonS`,
-- `hastyPersonS` sequence

regularPersonS :: State TState [TOutput]
regularPersonS = sequence [coinS, pushS]

distractedPersonS :: State TState [TOutput]
distractedPersonS = sequence [coinS]

hastyPersonS :: State TState [TOutput]
hastyPersonS = do
  a0 <- pushS
  case a0 of
    Thanks -> error "unreachable"
    Open -> return [a0]
    Tut -> do
      a1 <- coinS
      return [a0, a1]

-- X: Implement `luckyPairS`

luckyPairS :: Bool -> State TState Bool
luckyPairS b =
  let n = bool regularPersonS distractedPersonS b
  in do
    _ <- n
    a1 <- pushS
    return $ a1 == Open

-- There's `put` to replace the current state

put :: s -> State s ()
put s = State $ const ((), s)

-- There's `get` to get the current state

get :: State s s
get = State $ \s -> (s, s)

-- There's `evalState` to run the state and return only the output

evalState :: State s a -> s -> a
evalState p = fst . runState p

-- There's `execState` to run the state and return only the state

execState :: State s a -> s -> s
execState p = snd . runState p

testTurnstile :: State TState Bool
testTurnstile = do
  put Locked
  check1 <- pushS
  put Unlocked
  check2 <- pushS
  put Locked
  return (check1 == Tut && check2 == Open)

-- GHCi> runState testTurnstile Locked
-- (True,Locked)
-- HHCi> runState testTurnstile Unlocked
-- (True,Locked)

-- X: Rewrite `coinS` using a do construct with get and/or set.

coinS' :: State TState TOutput
coinS' = do
  put Unlocked
  return Thanks

-- X: Extend `testTurnstile` so that it also checks the state is set
-- to Unlocked after a coin is inserted, regardless of the state
-- beforehand. And for good measure, have `testTurnstile` return the
-- turnstile to it's original state when the testing is complete.

testTurnstile' :: State TState Bool
testTurnstile' = do
  initial <- get
  put Locked
  check1 <- pushS
  put Unlocked
  check2 <- pushS
  put Locked
  _ <- coinS
  check3 <- get
  put Unlocked
  _ <- coinS
  check4 <- get
  put initial
  return (check1 == Tut &&
          check2 == Open &&
          check3 == Unlocked &&
          check4 == Unlocked)

-- λ> runState testTurnstile' Locked
-- (True,Locked)
-- λ> runState testTurnstile' Unlocked
-- (True,Unlocked)

-- X: Write an implementation of
-- `modify :: (s -> s) -> State s ()`
-- which modifies the current state using a function

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- X: Write an implementation of
-- `gets :: (s -> a) -> State s a`
-- which produces a modified copy of the state while
-- leaving the state itself unchanged.

gets :: (s -> a) -> State s a
gets f = State $ \s -> (f s, s)

-- Monadic Control Structure

-- replicateM :: Applicative m => Int -> m a -> m [a]

-- evalState (replicateM 6 pushS) Unlocked
-- [Open,Tut,Tut,Tut,Tut,Tut]

-- To apply more control we need a better way to run the finite state
-- machine

data TInput = Coin | Push deriving (Eq, Show)

turnS :: TInput -> State TState TOutput
turnS = state . turn where
  turn Coin _        = (Thanks, Unlocked)
  turn Push Unlocked = (Open, Locked)
  turn Push Locked   = (Tut, Locked)

-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

-- evalState (mapM turnS [Coin, Push, Push, Coin, Push]) Locked
-- [Thanks,Open,Tut,Thanks,Open]

-- turnS is a /Transducer/: it converts an ordered sequence of inputs
-- to an ordered sequence of outputs, maintaining the state as it goes
-- along.

-- filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]

getsThroughS :: TInput -> State TState Bool
getsThroughS i = do
  o <- turnS i
  return $ o == Open

-- evalState (filterM getsThroughS [Coin, Push, Push, Coin, Push]) Locked
-- [Push,Push]

-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b

countOpens :: [TInput] -> State TState Int
countOpens = foldM f 0
  where f c i = bool c (c + 1) <$> getsThroughS i

  -- where f c i = do
  --         g <- getsThroughS i
  --         return $ bool c (c + 1) g

  -- where f c i = turnS i >>= inc c
  --       inc c = \case
  --         Open -> return $ c + 1
  --         _ -> return c

-- evalState (countOpens [Coin, Push, Coin, Push, Push, Coin, Push]) Locked
-- 3

-- X: Modify regularPersonS, distractedPersonS and hastyPersonS to use
-- turnS and mapM

regularPersonS' :: State TState [TOutput]
regularPersonS' = mapM turnS [Coin, Push]

distractedPersonS' :: State TState [TOutput]
distractedPersonS' = mapM turnS [Coin]

hastyPersonS' :: State TState [TOutput]
hastyPersonS' = do
  o1 <- turnS Push
  case o1 of
    Thanks -> error "unreachable"
    Open -> return [Open]
    Tut -> do
      o2 <- turnS Coin
      return [Tut, o2]

  -- o1 <- mapM turnS [Push]
  -- case o1 of
  --   [Open] -> return o1
  --   [Tut] -> do
  --     o2 <- mapM turnS [Coin]
  --     return (Tut : o2)

-- X: Implement tuesdayS using sequence or mapM.

tuesdayS :: State TState [TOutput]
tuesdayS = mconcat <$> sequence [ regularPersonS
                                , hastyPersonS
                                , distractedPersonS
                                , hastyPersonS
                                ]

-- λ> tuesday Locked
-- ([Thanks,Open,Tut,Thanks,Thanks,Open],Locked)
-- λ> runState tuesdayS Locked
-- ([Thanks,Open,Tut,Thanks,Thanks,Open],Locked)
-- λ> tuesday Locked == runState tuesdayS Locked
-- True

-- X: Implement `saveCoinsS :: [TInput] -> State TState Int` that
-- potentially processes all of the given inputs, but will skip a Coin
-- input if the previous input generated a Thanks. It returns the
-- number of Coin inputs that were skipped. E.g. evalState (saveCoins
-- [Push, Coin, Coin, Coin, Push, Push, Coin, Push]) Locked should
-- give 2.

saveCoinsS :: [TInput] -> State TState Int
saveCoinsS i = snd <$> foldM f (Nothing, 0) i
  where f :: (Maybe TOutput, Int) -> TInput -> State TState (Maybe TOutput, Int)
        f (Just Thanks, n) Coin = return (Just Thanks, n + 1)
        f (_, n) i = (, n) . Just <$> turnS i
        -- f (_, n) i = do
        --   o <- turnS i
        --   return (Just o, n)

-- evalState (saveCoinsS [Push, Coin, Coin, Coin, Push, Push, Coin, Push]) Locked == 2

-- X: Implement `sequenceUntil :: (a -> Bool) -> [State s a] -> State
-- s [a]`. It processes each of the inputs until one of them generates
-- a value that matches the predicate, then processes no
-- more. E.g. evalState (sequenceUntil (== Open) [coinS, coinS, coinS,
-- pushS, pushS, coinS]) Locked should give [Thank,Thank,Thank,Open]

sequenceUntil :: (a -> Bool) -> [State s a] -> State s [a]
sequenceUntil _ [] = return []
sequenceUntil p (sa:sas) = do
  a <- sa
  if p a
    then return [a]
    else do
      as <- sequenceUntil p sas
      return (a:as)

-- evalState (sequenceUntil (== Open) [coinS, coinS, coinS, pushS, pushS, coinS]) Locked == [Thank,Thank,Thank,Open]

-- X: Modify sequenceUntil so that it works with any Monad instance

sequenceUntilM :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntilM _ [] = return []
sequenceUntilM p (ma:mas) = do
  a <- ma
  if p a
    then return [a]
    else do
      as <- sequenceUntilM p mas
      return (a:as)

-- Pseudo Random Number Generator

-- let s = mkStdGen 666
-- random s :: (Int, StdGen)
-- (6438947685955577547,392509921 2103410263)

-- Suppose we want a function that rolls two dice and returns a pair
-- representing the result of each throw. Here's one way

rollPair :: StdGen -> ((Int, Int), StdGen)
rollPair g = let (n, g') = randomR (1, 6) g
                 (m, g'') = randomR (1, 6) g'
                 in ((n, m), g'')

-- X: Implement rollSix :: StdGen -> ([Int], StdGen) using randomR
-- (1,6) that returns a list representing the result of six
-- consecutive throws.

rollSix :: StdGen -> ([Int], StdGen)
rollSix g0 = let r = randomR (1, 6)
                 (n1, g1) = r g0
                 (n2, g2) = r g1
                 (n3, g3) = r g2
                 (n4, g4) = r g3
                 (n5, g5) = r g4
                 (n6, g6) = r g5
                 in ([n1, n2, n3, n4, n5, n6], g6)

-- X: Implement rollN :: Int -> StdGen -> ([Int], StdGen). This is a
-- bit tricky! But possible using iterate and take, for example.

rollN :: Int -> StdGen -> ([Int], StdGen)
rollN 0 g = ([], g)
rollN n g = let (r, g') = randomR (1, 6) g
                (rs, g'') = rollN (n - 1) g'
            in (r:rs, g'')
-- rollN 0 g = ([], g)
-- rollN n g = let xs = tail $ take (n + 1) $ iterate (randomR (1, 6) . snd) (0, g)
--             in (fst <$> xs, last (snd <$> xs))


-- X: We're about to define rollDieS :: State StdGen Int. Why don't
-- you have a go at it first, and contemplate what it is and how it
-- could help.

rollDieS :: State StdGen Int
rollDieS = state $ randomR (1, 6)

-- Now you can write in do notation

rollPairS :: State StdGen (Int, Int)
rollPairS = do
  x <- rollDieS
  y <- rollDieS
  return (x, y)

-- X: Implement rollSixS :: State StdGen [Int] with the same behaviour
-- as rollSix. Use do notation and rollDieS.

rollSixS :: State StdGen [Int]
rollSixS = replicateM 6 rollDieS

-- X: Implement rollNS :: Int -> StdGen -> State StdGen [Int] using
-- replicateM

rollNS :: Int -> State StdGen [Int]
rollNS n = replicateM n rollDieS

-- X: Implement luckyDoubleS :: State StdGen Int. It does a first
-- throw. If it's a 6 it throws again and returns the total of the two
-- throws, else it just returns the first throw.

luckyDoubleS :: State StdGen Int
luckyDoubleS = do
  n <- rollDieS
  if n == 6
    then rollDieS <&> (n+)
    else return n

-- State is also a Functor and an Applicative

-- X: Rewrite rollPairS using (<$>) and (<*>), or liftA2

rollPairS' :: State StdGen (Int, Int)
rollPairS' = liftA2 (,) rollDieS rollDieS


-- X: Implement happyDoubleS :: State StdGen Int, which throws two
-- dice and returns the sum of the first and the second, but doubles
-- the total if the first is a six. Code it using do notation

happyDoubleS :: State StdGen Int
happyDoubleS = do
  n <- rollDieS
  m <- rollDieS
  return $ n + m * bool 1 2 (n == 6)

-- X: Rewrite happyDoubleS using (<$>) and (<*>), or liftA2.

happyDoubleS' :: State StdGen Int
happyDoubleS' = liftA2 f rollDieS rollDieS
  where f n m = n + m * bool 1 2 (n == 6)

-- X: Can you recode luckyDoubleS using just (<$>) and (<*>) (or
-- liftA2)? Why not? Can you use (<$>) (or fmap) to make it a bit
-- shorter?

-- luckyDoubleS: it does a first throw. If it's a 6 it throws again
-- and returns the total of the two throws, else it just returns the
-- first throw.

luckyDoubleS' :: State StdGen Int
luckyDoubleS' = do
  n <- rollDieS
  if n == 6
    then (n+) <$> rollDieS
    else return n

-- X: Modify tuesdayS, saveCoins and sequenceUntil from the Monadic
-- Control Structures exercises using fmap and/or (<$>).

-- tuesdayS already did
-- saveCoinsS already did

sequenceUntil' :: (a -> Bool) -> [State s a] -> State s [a]
sequenceUntil' _ [] = return []
sequenceUntil' p (sa:sas) = do
  a <- sa
  if p a
    then return [a]
    else (a:) <$> sequenceUntil' p sas

-- Because State StdGen is "agnostic" in regard to the type of the
-- pseudo-random value it produces, we can write a similarly
-- "agnostic" function that provides a pseudo-random value of
-- unspecified type (as long as it is an instance of Random):

getRandomS :: Random a => State StdGen a
getRandomS = state random

someTypes :: State StdGen (Int, Bool, Float)
someTypes = liftA3 (,,) getRandomS getRandomS getRandomS

-- X: Write randomElt :: [a] -> State StdGen a, using put and get to
-- access the StdGen state. It can assume the list is non empty, and
-- should return a random element from within it.

randomElt :: [a] -> State StdGen a
randomElt as = do
  g <- get
  let (i, g') = randomR (0, length as - 1) g
  put g'
  return $ as !! i

-- X: Rewrite randomElt without using put or get.

randomElt' :: [a] -> State StdGen a
randomElt' as = do
  i <- state $ randomR (0, length as - 1)
  return $ as !! i
