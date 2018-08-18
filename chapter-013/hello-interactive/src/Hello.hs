module Hello where

sayHello :: String -> IO ()
sayHello name = do
  putStrLn ("Hello " ++ name ++ "!")
