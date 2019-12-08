module BuildingFunctions where

bang :: String -> String
bang s = s ++ "!"

at4 :: String -> String
at4 s = take 1 $ drop 4 s

drop9 :: String -> String
drop9 s = drop 9 s
