module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "Woooot"]

-- More specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- What happens if we lift it again?
liftedReplaceWithP :: Functor f => f a -> f Char
liftedReplaceWithP = fmap replaceWithP

-- More specific
liftedReplaceWithP' :: [Maybe [Char]] -> [Char]
liftedReplaceWithP' = liftedReplaceWithP

-- What happens if we lift it again?
twiceLiftedReplaceWithP :: (Functor f, Functor g) => f (g a) -> f (g Char)
twiceLiftedReplaceWithP = (fmap . fmap) replaceWithP
-- Or equivalently
-- twiceLiftedReplaceWithP = fmap liftedReplaceWithP

-- More specific
twiceLiftedReplaceWithP' :: [Maybe [Char]] -> [Maybe Char]
twiceLiftedReplaceWithP' = twiceLiftedReplaceWithP

-- What happens if we lift it again?
thriceLiftedReplaceWithP :: (Functor f, Functor g, Functor h) => h (f (g a)) -> h (f (g Char))
thriceLiftedReplaceWithP = (fmap . fmap . fmap) replaceWithP
-- Or equivalently
-- thriceLiftedReplaceWithP = fmap twiceLiftedReplaceWithP

-- More specific
thriceLiftedReplaceWithP' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLiftedReplaceWithP' = thriceLiftedReplaceWithP

main :: IO ()
main = do
  putStr "replaceWithP lms: "
  print (replaceWithP lms)

  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)

  putStr "liftedReplaceWithP lms: "
  print (liftedReplaceWithP lms)

  putStr "liftedReplaceWithP' lms: "
  print (liftedReplaceWithP' lms)

  putStr "twiceLiftedReplaceWithP lms: "
  print (twiceLiftedReplaceWithP lms)

  putStr "twiceLiftedReplaceWithP' lms: "
  print (twiceLiftedReplaceWithP' lms)

  putStr "thriceLiftedReplaceWithP lms: "
  print (thriceLiftedReplaceWithP lms)

  putStr "thriceLiftedReplaceWithP' lms: "
  print (thriceLiftedReplaceWithP' lms)
