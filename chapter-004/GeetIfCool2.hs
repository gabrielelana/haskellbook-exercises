
module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
  then putStrLn "eyyyy. What's shakin'?"
  else putStrLn "pshhhh."
  where
    cool v = v == "downright frosty yo"

main :: IO ()
main =
  do
    greetIfCool "ah"
    greetIfCool "downright frosty yo"
