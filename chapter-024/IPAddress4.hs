module IPAddress4 (IPAddress4, parse, fromComponents, toComponents) where

import Text.Trifecta
import Text.Parser.Combinators
import Data.Word
import Data.Bits
import Data.List
import Control.Monad

newtype IPAddress4 = IPAddress4 Word32 deriving (Eq, Ord)

instance Show IPAddress4 where
  show a@(IPAddress4 w) = mconcat $ intersperse "." $ show <$> toComponents a

toComponents :: IPAddress4 -> [Word8]
toComponents (IPAddress4 w) = reverse $ octectAt w <$> [0..3]
  where
    octectAt w n = fromIntegral $ shiftR (shiftL mask (n * 8) .&. w) (n * 8)
    mask = 0xff

fromComponents :: [Word8] -> IPAddress4
fromComponents ns = IPAddress4 (toWord32 ns)
  where toWord32 ns = foldr f (0::Word32) (zip [0..] ns)
        f (i, n) r = r .|. (fromIntegral n `shiftL` (8 * (3 - i)))

parse :: String -> Result IPAddress4
parse = parseString parseIPAddress mempty

parseIPAddress :: Parser IPAddress4
parseIPAddress = do
  ns <- (read <$> some digit) `sepBy` char '.'
  when (length ns /= 4) (fail "IPv4 addresses must be composed of 4 octects")
  when (any invalidOctect ns) (fail "IPv4 octect must be between 0 and 255")
  return $ fromComponents ns
  where
    invalidOctect n = n < 0 || n > 255
