module IPAddress6 (IPAddress6, parse, fromComponents, toComponents) where

import Text.Trifecta
import Text.Parser.Combinators
import Data.Word
import Data.Bits
import Data.List
import Control.Monad
import Numeric (showHex)

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

instance Show IPAddress6 where
  show a = mconcat $ intersperse ":" (flip showHex "" <$> toComponents a)

toComponents :: IPAddress6 -> [Word16]
toComponents (IPAddress6 w1 w2) = blocks w1 <> blocks w2
  where blocks w = reverse $ fromIntegral . blockAt w <$> [0..3]
        blockAt w n = shiftR (shiftL mask (n * 16) .&. w) (n * 16)
        mask = 0xffff

parse :: String -> Result IPAddress6
parse = parseString parseIPAddress6 mempty . expandIPAddress6

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
  ns <- (read . ("0x" <>) <$> some hexDigit) `sepBy` char ':'
  when (length ns /= 8) (fail "IPv6 addresses must be composed of 8 blocks")
  return $ fromComponents ns

fromComponents :: [Word16] -> IPAddress6
fromComponents ns = IPAddress6 (toWord64 $ take 4 ns) (toWord64 $ drop 4 ns)
  where toWord64 ns = foldr f (0::Word64) (zip [0..] ns)
        f (i, n) r = r .|. (fromIntegral n `shiftL` (16 * (3 - i)))

expandIPAddress6 :: String -> String
expandIPAddress6 s = let isContracted = isInfixOf "::" s
                         n = length $ filter (':' ==) s
                         r = intersperse ':' $ replicate (8 - n) '0'
                     in if isContracted
                        then replace "::" (":" <> r <> ":") s
                        else s

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "first argument cannot be empty"
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace from to [] = []
