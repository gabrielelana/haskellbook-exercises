module Main where

import Morse
import Test.QuickCheck
import qualified Data.Map as M


allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.keys morseToLetter

charGenerator :: Gen Char
charGenerator = elements allowedChars

morseGenerator :: Gen Morse
morseGenerator = elements allowedMorse

property_ThereAndBackAgain :: Property
property_ThereAndBackAgain =
  forAll charGenerator (\c -> (charToMorse c >>= morseToChar) == (Just c))


main :: IO ()
main = quickCheck property_ThereAndBackAgain
