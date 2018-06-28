
module SumsUpTo where

sumsUpTo :: (Eq a, Num a) => a -> a
sumsUpTo 0 = 0
sumsUpTo 1 = 1
sumsUpTo n = n + sumsUpTo (n - 1)
