
module MaybeDividedBy where

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

data DividedByResult
  = Result Quotient
  | DividedByZero
  deriving Show

dividedBy :: Numerator -> Denominator -> DividedByResult
dividedBy n 0 = DividedByZero
dividedBy n d = Result $ div n d
