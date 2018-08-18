module Person where

import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)
import Text.Read (readMaybe)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age


-- It should prompt the user for a name and age input.
-- It should attempt to construct a Person value using the name
-- and age the user entered.
-- If it constructed a successful person, it should print "Yay!
-- Successfully got a person: " followed by the Person value.
-- If it got an error value, report that an error occurred and
-- print the error.

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Person name: "
  name <- getLine
  putStr "Person age: "
  maybeAge <- getLine
  case readMaybe maybeAge :: Maybe Integer of
    Nothing ->
      putStrLn $ "error: `" ++ maybeAge ++ "` is not a number"
    (Just age) ->
      case mkPerson name age of
        (Left NameEmpty) ->
          putStrLn "error: the person name cannot be empty"
        (Left AgeTooLow) ->
          putStrLn "error: the person age cannot be too low"
        (Left (PersonInvalidUnknown reason)) ->
          putStrLn $ "error: " ++ reason
        (Right person) ->
          putStrLn $ "Yay! Successfully fot a person: " ++ show person
