module PoemLines where

import Prelude hiding (lines)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
lines :: String -> [String]
lines "" = []
lines s = (takeWhile (/= '\n') s) : lines ((dropWhile (== '\n') . dropWhile (/= '\n')) s)

-- What we want 'lines sentences'
-- to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

-- The main function here is a small test -- to ensure you've written your function -- correctly.
main :: IO ()
main = print $
       "Are they equal? "
       ++ show (lines sentences == shouldEqual)
