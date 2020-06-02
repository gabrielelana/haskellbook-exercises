module DayOfWeek where

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date dayOfWeek dayOfMonth)
       (Date dayOfWeek' dayOfMonth') =
    dayOfWeek == dayOfWeek' && dayOfMonth == dayOfMonth'
