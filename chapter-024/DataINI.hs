module DataINI where

import Text.Trifecta
import Text.Parser.Combinators
import qualified Data.Map as M

type Name = String
type Value = String
type Assignments = M.Map Name Value

newtype Header = Header String
               deriving (Eq, Ord, Show)

newtype Config = Config (M.Map Header Assignments)
               deriving (Eq, Show)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseBetweenSquareBraket :: Parser a -> Parser a
parseBetweenSquareBraket p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBetweenSquareBraket $ Header <$> some letter

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  value <- some (noneOf "\n")
  skipEOL
  return (name, value)

skipComment :: Parser ()
skipComment = skipSome (oneOf ";#") >> skipMany (noneOf "\n") >> skipEOL

skipWhitespace :: Parser ()
skipWhitespace = () <$ oneOf " \n"

parseSection :: Parser (Header, Assignments)
parseSection = do
  many skipWhitespace
  many skipComment
  header <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return (header, M.fromList assignments)

parseINI :: Parser Config
parseINI = do
  sections <- some parseSection
  return $ Config (M.fromList sections)

parse :: String -> Result Config
parse = parseString parseINI mempty
