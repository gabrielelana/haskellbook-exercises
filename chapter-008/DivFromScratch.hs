
module DivFromScratch where

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy n d = go n d 0
  where go n d c
          | n < d = c
          | otherwise = go (n - d) d (c + 1)
