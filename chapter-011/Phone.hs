module Phone where

import Data.Char (isUpper, toLower)
import Data.List (maximumBy, group, sort)
import Data.Foldable (concat)

data Action = Display Char
            | ToUpper
            deriving (Eq, Show)

type Label = Char
type Taps = Int

data Button = Button Label [Action] deriving (Eq, Show)

data Phone = Phone [Button] deriving (Eq, Show)

phone = Phone [
  Button '1' [Display '1'],
  Button '2' [Display 'a', Display 'b', Display 'c', Display '2'],
  Button '3' [Display 'd', Display 'e', Display 'f', Display '3'],
  Button '4' [Display 'g', Display 'h', Display 'i', Display '4'],
  Button '5' [Display 'j', Display 'k', Display 'l', Display '5'],
  Button '6' [Display 'm', Display 'n', Display 'o', Display '6'],
  Button '7' [Display 'p', Display 'q', Display 'r', Display 's', Display '7'],
  Button '8' [Display 't', Display 'u', Display 'v', Display '8'],
  Button '9' [Display 'w', Display 'x', Display 'y', Display 'z', Display '9'],
  Button '0' [Display ' ', Display '+', Display '0'],
  Button '*' [ToUpper, Display '^', Display '*'],
  Button '#' [Display '.', Display ',', Display '#']]

-- conversations :: [String]
-- conversations =
--   ["Wanna play 20 questions",
--     "Ya",
--     "U 1st haha",
--     "Lol ok. Have u ever tasted alcohol",
--     "Lol ya",
--     "Wow ur cool haha. Ur turn",
--     "Ok. Do u think I am pretty Lol",
--     "Lol ya",
--     "Just making sure rofl ur turn"]

possibleTaps :: Phone -> [(Label, Taps, Action)]
possibleTaps (Phone buttons) = concatMap buttonTaps buttons
  where buttonTaps (Button label actions) =
          zipWith (\n action -> (label, n, action)) [1..] actions

tapsFor :: [(Label, Taps, Action)] -> Action -> (Label, Taps)
tapsFor [] _ = error "character not representable with phone"
tapsFor ((label, taps, ToUpper):rest) ToUpper = (label, taps)
tapsFor ((label, taps, ToUpper):rest) a = tapsFor rest a
tapsFor ((label, taps, Display c):rest) ToUpper = tapsFor rest ToUpper
tapsFor ((label, taps, Display c1):rest) (Display c2)
  | c1 == c2 = (label, taps)
  | otherwise = tapsFor rest (Display c2)

reverseTaps :: Phone -> Char -> [(Label, Taps)]
reverseTaps phone c
  | isUpper c = (tapsFor (possibleTaps phone) ToUpper) : reverseTaps phone (toLower c)
  | otherwise = [tapsFor (possibleTaps phone) (Display c)]

toTaps :: Phone -> String -> [(Label, Taps)]
toTaps phone s = concatMap (reverseTaps phone) s

-- conversationsToTaps = map (toTaps phone) conversations

fingerTaps :: [(Label, Taps)] -> Taps
fingerTaps = foldr ((+) . snd) 0

costToTapForChar :: Phone -> Char -> Int
costToTapForChar phone c = sum $ map snd (reverseTaps phone c)

costOfMostPopularLetter :: Phone -> String -> Int
costOfMostPopularLetter phone s = (costToTapForChar phone (fst m)) * (snd m)
  where m = mostPopularLetterWithWeight s

mostPopularLetter :: String -> Char
mostPopularLetter s = fst $ mostPopularLetterWithWeight s

mostPopularLetterWithWeight :: String -> (Char, Int)
mostPopularLetterWithWeight s =
  maximumBy (\x y -> compare (snd x) (snd y)) $
  map (\s -> (head s, length s)) $
  group $
  sort s

mostPopularWord :: String -> String
mostPopularWord s =
  fst $
  maximumBy (\x y -> compare (snd x) (snd y)) $
  map (\s -> (head s, length s)) $
  group $
  sort $
  words s

-- What was the most popular letter for each message?
-- mostPopularLetterForEachMessage = map mostPopularLetter conversations

-- What was its cost?
-- costOfMostPopularLetterForEachMessage = map (costOfMostPopularLetter phone) conversations

-- What was the most popular letter overall?
-- mostPopularLetterOverall = mostPopularLetter (concat conversations)

-- What was the most popular word?
-- mostPopularWordOverall = mostPopularWord (concat conversations)
