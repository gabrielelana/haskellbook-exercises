module Cipher where

import Data.Char

encode :: Int -> String -> String
encode n s = map (encodeChar n) s

decode :: Int -> String -> String
decode n s = map (decodeChar n) s

decodeChar :: Int -> Char -> Char
decodeChar n = encodeChar (-n)

encodeChar :: Int -> Char -> Char
encodeChar n c
  | elem c ['a'..'z'] = shift n c
  | otherwise = c

shift :: Int -> Char -> Char
shift n c = chr ((mod ((ord c - base) + n) l) + base)
  where l = (ord 'z') - (ord 'a') + 1
        base = ord 'a'
