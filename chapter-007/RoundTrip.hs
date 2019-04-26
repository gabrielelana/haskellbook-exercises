module RoundTrip where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)
