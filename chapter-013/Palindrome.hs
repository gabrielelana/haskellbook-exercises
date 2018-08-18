module Palindrome where

import Control.Monad
import Data.Char (toLower)
import System.Exit (exitSuccess)

isPalindrome :: String -> Bool
isPalindrome s = (reverse canonicalForm) == canonicalForm
  where canonicalForm = canonicalFormOf s
        canonicalFormOf s = filter inAlphabet (map toLower s)
        inAlphabet = (flip elem) "abcdefghjkilmnopqrstuvwxyz"

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  case (isPalindrome line) of
    True ->
      putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
