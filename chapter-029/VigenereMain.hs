module VigenereMain where

import Cipher
import System.Environment
import System.IO
import Control.Monad (replicateM, forever, MonadPlus (mzero))
import Control.Exception (catch, throw)
import System.IO.Error (isEOFError)
import Data.Functor ((<&>))
import Data.Char (ord)
import Text.Read (readMaybe)
import System.Exit (die)

readNow :: Handle -> IO String
readNow = readTimeout 0 -- do not wait

readBlock :: Handle -> IO String
readBlock = readTimeout (-1) -- wait forever

readTimeout :: Int -> Handle -> IO String
readTimeout n h = reverse <$> go ""
  where go s = do
          -- Wait until there are characters available or a number of
          -- milliseconds passed or is the EOF
          ready <- hWaitForInput h n `catch` handleEOF
          -- If time is out or is the end of file then we finished to
          -- red the input
          if not ready then return s else do
            -- Handle the EOT character (C-d on unix)
            eof <- hIsEOT h
            if eof then return s else do
              c <- hGetChar h
              go (c:s)
        handleEOF e
          | isEOFError e = return False
          | otherwise = throw e

hIsEOT :: Handle -> IO Bool
hIsEOT h = do
  eof <- hIsEOF h
  if eof then return True else do
    next <- hLookAhead h
    return $ isEOT next
  where isEOT = (== 4) . ord

usage :: String -> String
usage n = "\n" <>
  "USAGE: " <> n <> "\n" <>
  "  -e [-t INT] = encode from STDIN until [INT seconds timeout or] EOF\n" <>
  "  -d          = decode from STDIN until EOF\n"

alphabet :: String
alphabet = ['A'..'Z']

keyword :: String
keyword = "ALLY"

decode :: String -> String
decode = vigenereDecode alphabet keyword

encode :: String -> String
encode = vigenereEncode alphabet keyword

type Timeout = Int

data Mode = Encode (Maybe Timeout)
          | Decode
          | Unknown
          deriving (Eq)

mode :: [String] -> Mode
mode ["-d"] = Decode
mode ["-e"] = Encode Nothing
mode ["-e", "-t", n] = case readMaybe n of
                         t@(Just _) -> Encode t
                         Nothing -> Unknown
mode _ = Unknown

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  isInteractive <- hIsTerminalDevice stdin
  name <- getProgName
  args <- getArgs
  case mode args of
    Decode -> do
      -- NOTE: to decode (as from spec) it shouldn't block, but if it
      -- isn't interactive, aka input not coming from user but from a
      -- pipe, we need to wait for the input to come through the pipe.
      -- Therefore we don't wait only if input comes from an user.
      s <- (if isInteractive then readNow else readBlock) stdin
      putStr $ decode s
    (Encode (Just t)) -> do
      s <- readTimeout (t * 1000) stdin
      putStr $ encode s
    (Encode _) -> do
      s <- readBlock stdin
      putStr $ encode s
    Unknown -> do
      die $ usage name
