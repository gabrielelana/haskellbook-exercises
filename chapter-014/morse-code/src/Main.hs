module Main where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Morse (stringToMorse, morseToChar)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hIsEOF, hGetLine, stdin)

convertToMorse :: IO ()
convertToMorse = forever $ do
  areWeDone <- hIsEOF stdin
  when areWeDone exitSuccess

  line <- hGetLine stdin
  convertLine line

  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just s) ->
          putStrLn (intercalate " " s)
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  areWeDone <- hIsEOF stdin
  when areWeDone exitSuccess

  line <- hGetLine stdin
  convertLine line

  where
    convertLine line = do
      let decoded :: Maybe String
          decoded = traverse morseToChar (words line)
      case decoded of
        (Just m) ->
          putStrLn m
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    ["from"] -> convertFromMorse
    ["to"] -> convertToMorse
    _ -> usage
  where usage = do
          putStrLn "Use: morse-code [to|from]"
          exitFailure
