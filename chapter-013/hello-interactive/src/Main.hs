module Main where

import Hello
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "What is your name? "
  name <- getLine
  sayHello name
