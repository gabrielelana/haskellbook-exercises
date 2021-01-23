{-# LANGUAGE OverloadedStrings #-}

module ActivityLog where

import Data.Text (pack, unpack, stripEnd)
import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative
import qualified Data.Map as M
import Data.Maybe (catMaybes)


data Day = Day { year :: Int
               , month :: Int
               , day :: Int
               } deriving (Eq, Ord, Show)

data Time = Time { hour :: Int
                 , minutes :: Int
                 } deriving (Eq, Ord, Show)

data Activity = Activity { startAt :: Time
                         , description :: String
                         } deriving (Eq, Ord, Show)

data Log = Log { on :: Day
               , activities :: [Activity]
               } deriving (Eq, Ord, Show)

eolP :: Parser ()
eolP = try commentP <|> () <$ newline

eoiP :: Parser ()
eoiP = eof <|> (() <$ some newline <* optional eof)

commentP :: Parser ()
commentP = () <$ (skipMany (char ' ') *> symbol "--" *> manyTill anyChar newline)

ignoreP :: Parser ()
ignoreP = commentP <|> (() <$ newline)

dayP :: Parser Day
dayP = do
  symbol "#"
  year <- read <$> many digit
  symbol "-"
  month <- read <$> many digit
  symbol "-"
  day <- read <$> many digit
  eolP
  return $ Day year month day

activityP :: Parser Activity
activityP = do
  h <- read <$> many digit
  symbol ":"
  m <- read <$> many digit
  spaces
  description <- unpack . stripEnd . pack <$> manyTill anyChar eolP
  return $ Activity (Time h m) description

logP :: Parser Log
logP = do
  optional $ many ignoreP
  on <- dayP
  activities <- some activityP
  eoiP
  return $ Log on activities

logsP :: Parser [Log]
logsP = some logP

fromFile :: String -> IO (Result [Log])
fromFile fp = parse <$> readFile fp

parse :: String -> Result [Log]
parse = parseString logsP mempty

duration :: [Activity] -> [(String, Int)]
duration = fst . foldr f ([], Time 24 0)
  where f (Activity t@(Time h1 m1) description) (r, Time h2 m2) =
          let duration = ((h2 - h1) * 60) - m1 + m2
          in ((description, duration):r, t)

sumActivitiesDuration :: [Log] -> M.Map String Int
sumActivitiesDuration ls = foldr f M.empty (mconcat (duration . activities <$> ls))
  where f :: (String, Int) -> M.Map String Int -> M.Map String Int
        f (s, d) m = case M.lookup s m of
                       Just ds -> M.insert s (d + ds) m
                       Nothing -> M.insert s d m

avgActivitiesDuration :: [Log] -> M.Map String Float
avgActivitiesDuration ls = let n = fromIntegral $ length ls
                               f = (/ n) . fromIntegral
                           in f <$> sumActivitiesDuration ls

-- daysOfActivity :: [Log] -> String -> Int
-- daysOfActivity ls d = length $ filter (inDay d) ls
--   where inDay :: String -> Log -> Bool
--         inDay d l = any ((d ==) . description) (activities l)

-- logsExample :: [Log]
-- logsExample =
--   [ Log { on = Day {year = 2025, month = 2, day = 5},
--           activities =
--             [ Activity {startAt = Time {hour = 8, minutes = 0}, description = "Breakfast"}
--             , Activity {startAt = Time {hour = 9, minutes = 0}, description = "Sanitizing moisture collector"}
--             , Activity {startAt = Time {hour = 11, minutes = 0}, description = "Exercising in high-grav gym"}
--             , Activity {startAt = Time {hour = 12, minutes = 0}, description = "Lunch"}
--             , Activity {startAt = Time {hour = 13, minutes = 0}, description = "Programming"}
--             , Activity {startAt = Time {hour = 17, minutes = 0}, description = "Commuting home in rover"}
--             , Activity {startAt = Time {hour = 17, minutes = 30}, description = "R&R"}
--             , Activity {startAt = Time {hour = 19, minutes = 0}, description = "Dinner"}
--             , Activity {startAt = Time {hour = 21, minutes = 0}, description = "Shower"}
--             , Activity {startAt = Time {hour = 21, minutes = 15}, description = "Read"}
--             , Activity {startAt = Time {hour = 22, minutes = 0}, description = "Sleep"}
--             ]
--         }
--   , Log { on = Day {year = 2025, month = 2, day = 7},
--           activities =
--             [ Activity {startAt = Time {hour = 8, minutes = 0}, description = "Breakfast"}
--             , Activity {startAt = Time {hour = 9, minutes = 0}, description = "Bumped head, passed out"}
--             , Activity {startAt = Time {hour = 13, minutes = 36}, description = "Wake up, headache"}
--             , Activity {startAt = Time {hour = 13, minutes = 37}, description = "Go to medbay"}
--             , Activity {startAt = Time {hour = 13, minutes = 40}, description = "Patch self up"}
--             , Activity {startAt = Time {hour = 13, minutes = 45}, description = "Commute home for rest"}
--             , Activity {startAt = Time {hour = 14, minutes = 15}, description = "Read"}
--             , Activity {startAt = Time {hour = 21, minutes = 0}, description = "Dinner"}
--             , Activity {startAt = Time {hour = 21, minutes = 15}, description = "Read"}
--             , Activity {startAt = Time {hour = 22, minutes = 0}, description = "Sleep"}
--             ]
--         }
--   ]
