module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dictionaryContent <- readFile "data/dict.txt"
  return (lines dictionaryContent)

wordIsTooShort :: String -> Bool
wordIsTooShort w = (length w) < 5

wordIsTooLong :: String -> Bool
wordIsTooLong w = (length w) > 9

gameWords :: IO WordList
gameWords = do
  words <- allWords
  return (filter (\w -> (not $ wordIsTooShort w) && (not $ wordIsTooLong w)) words)

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] deriving (Eq)

createPuzzle :: String -> Puzzle
createPuzzle s = Puzzle s (hidden s) []
  where hidden = map (const Nothing)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (map showDiscovered discovered)
    ++ " Guessed so far: " ++ guessed

showDiscovered :: Maybe Char -> Char
showDiscovered Nothing = '_'
showDiscovered (Just c) = c

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s) c = elem c s

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle wordToGuess discoveredSoFar guessedSoFar) guessed =
  Puzzle wordToGuess filled (guessed : guessedSoFar)
  where zipper guessed charInWord charMaybeDiscovered
          | guessed == charInWord = Just guessed
          | otherwise = charMaybeDiscovered
        filled = zipWith (zipper guessed) wordToGuess discoveredSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else..."
      return puzzle
    (True, _) -> do
      putStrLn "You guessed right!"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "You guessed wrong! Try again ;-)"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessedSoFar) =
  if (wrongGuesses guessedSoFar wordToGuess) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else
    return ()
  where notIn s = not . ((flip elem) s)
        wrongGuesses guessedSoFar wordToGuess =
          length $ filter (notIn wordToGuess) guessedSoFar

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discoveredSoFar _) =
  if all isJust discoveredSoFar then
    do putStrLn "You win!"
       exitSuccess
  else
    return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Puzzle: " ++ (show puzzle)
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] ->
      handleGuess puzzle c >>= runGame
    _ ->
      putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = createPuzzle word
  runGame puzzle
