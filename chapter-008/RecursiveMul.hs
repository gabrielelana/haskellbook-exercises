
module RecursiveMul where

recursiveSum :: Integral a => a -> a -> a
recursiveSum x 0 = x
recursiveSum x y = recursiveSum (x + 1) (y - 1)

recursiveMul :: Integral a => a -> a -> a
recursiveMul x y = go x y 0
  where go x y s
          | y == 0 = s
          | otherwise = go x (y - 1) (recursiveSum x s)
