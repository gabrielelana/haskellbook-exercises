module Main where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy n d = go n d 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

recursiveSum :: Integral a => a -> a -> a
recursiveSum x 0 = x
recursiveSum x y = recursiveSum (x + 1) (y - 1)

recursiveMul :: Integral a => a -> a -> a
recursiveMul x y = go x y 0
  where go x y s
          | y == 0 = s
          | otherwise = go x (y - 1) (recursiveSum x s)


main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $
      (1 + 1) > 1 `shouldBe` True
    it "1 + 2 is equal to 4" $
      (2 + 2) `shouldBe` 4
    it "15 divided by 3 is 5" $
      15 `dividedBy` 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $
      22 `dividedBy` 5 `shouldBe` (4, 2)
  describe "recursiveMul" $ do
    it "2 * 2 is 4" $
      recursiveMul 2 2 `shouldBe` 4
    it "2 * 4 is 8" $
      recursiveMul 2 4 `shouldBe` 8
  describe "property example" $
    it "x + 1 will always be greater than x" $
      property $ \x -> x + 1 > (x :: Int)
