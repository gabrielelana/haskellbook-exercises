module Print where

greetings :: String
greetings = "Hello World!"

hello :: String
hello = "Hello"

world :: String
world = "World!"

main :: IO ()
main = do
  putStrLn greetings
  putStrLn alternativeGreetings
  where alternativeGreetings = concat [hello, " ", world]
