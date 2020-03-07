module SumsUpTo where

sumsUpTo :: (Eq a, Num a) => a -> a
sumsUpTo 0 = 0
sumsUpTo 1 = 1
sumsUpTo n = n + sumsUpTo (n - 1)

sumsUpTo' :: (Eq a, Num a) => a -> a
sumsUpTo' n = up 1 n
  where up m n
          | m == n    = n
          | otherwise = m + (up (m + 1) n)
