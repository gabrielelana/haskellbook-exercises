module Cipher where

import Data.Char (ord, chr)
import Data.List (head, reverse, cycle, zip)
import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)
import Text.Read (readMaybe)

type Alphabet = [Char]

vigenereEncode :: Alphabet -> String -> String -> String
vigenereEncode a keyword message = map (vigenereEncodeChar a) (zip message keywordForMessage)
  where keywordForMessage = replaceWithKeyword a keyword message

vigenereDecode :: Alphabet -> String -> String -> String
vigenereDecode a keyword message = map (vigenereDecodeChar a) (zip message keywordForMessage)
  where keywordForMessage = replaceWithKeyword a keyword message

vigenereEncodeChar :: Alphabet -> (Char, Char) -> Char
vigenereEncodeChar a (mc, kc) = caesarEncodeChar a ((ord kc) - (ord $ head a)) mc

vigenereDecodeChar :: Alphabet -> (Char, Char) -> Char
vigenereDecodeChar a (mc, kc) = caesarDecodeChar a ((ord kc) - (ord $ head a)) mc

replaceWithKeyword :: Alphabet -> String -> String -> String
replaceWithKeyword a keyword message = replace a (cycle keyword) message
  where replace a _ [] = []
        replace a (kc : kt) (mc : mt)
          | canBeEncoded a mc = kc : (replace a kt mt)
          | otherwise = mc : (replace a (kc : kt) mt)

caesarEncode :: Alphabet -> Int -> String -> String
caesarEncode a n s = map (caesarEncodeChar a n) s

caesarDecode :: Alphabet -> Int -> String -> String
caesarDecode a n s = map (caesarDecodeChar a n) s

caesarEncodeChar :: Alphabet -> Int -> Char -> Char
caesarEncodeChar a n c
  | canBeEncoded a c = shift a n c
  | otherwise = c

caesarDecodeChar :: Alphabet -> Int -> Char -> Char
caesarDecodeChar a n c = caesarDecodeChar a (-n) c

canBeEncoded :: Alphabet -> Char -> Bool
canBeEncoded a c = elem c a

shift :: Alphabet -> Int -> Char -> Char
shift a n c = chr ((mod ((ord c) - base + n) range) + base)
  where base = (ord $ head a)
        range = (ord $ head $ reverse a) - base + 1

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let alphabet = "abcdefghjkilmnopqrstuvwxyz"
  putStr "Choose a cipher (Vigenere|Caesar): "
  cipher <- getLine
  case cipher of
    "Vigenere" -> do
      putStr "Choose a keyword: "
      keyword <- getLine
      putStr "Write a message to encode: "
      message <- getLine
      putStrLn $ vigenereEncode alphabet keyword message
    "Caesar" -> do
      putStr "Choose a number: "
      maybeShift <- getLine
      return ()
      case readMaybe maybeShift :: Maybe Int of
        (Just shift) -> do
          putStr "Write a message to encode: "
          message <- getLine
          putStrLn $ caesarEncode alphabet shift message
        Nothing -> do
          putStrLn $ "`" ++ maybeShift ++ "` is not a number"
    _ -> do
      putStrLn $ "`" ++ cipher ++ "` is not a valid cipher"
