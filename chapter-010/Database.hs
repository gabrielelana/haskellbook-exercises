module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = foldr accumulateDates [] db
  where accumulateDates item times =
          case item of
            DbDate time -> time : times
            _ -> times

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr accumulateNumbers [] db
  where accumulateNumbers item numbers =
          case item of
            DbNumber n -> n : numbers
            _ -> numbers

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb theDatabase = (fromIntegral (sum numbers)) / (fromIntegral (length numbers))
  where numbers = (filterDbNumber theDatabase)
