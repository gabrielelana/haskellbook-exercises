module Main where

import User
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import qualified Data.Text as T
import Text.Read (readMaybe)

usage :: String -> String
usage n = "\n" <>
  "USAGE: " <> n <> "\n" <>
  "  init\n" <>
  "  insert USERNAME SHELL HOME NAME PHONE\n" <>
  "  update USERID USERNAME SHELL HOME NAME PHONE\n"

main :: IO ()
main = do
  name <- getProgName
  args <- getArgs
  conn <- connect
  case args of
    ["init"] ->
      initDatabase conn
    ["insert", username_, shell_, homeDirectory_, realName_, phone_] ->
      let u = User
              Nothing
              (T.pack username_)
              (T.pack shell_)
              (T.pack homeDirectory_)
              (T.pack realName_)
              (T.pack phone_)
      in insertUser conn u
    ["update", userId_, username_, shell_, homeDirectory_, realName_, phone_] ->
      let u = User
              (readMaybe userId_)
              (T.pack username_)
              (T.pack shell_)
              (T.pack homeDirectory_)
              (T.pack realName_)
              (T.pack phone_)
      in updateUser conn u
    _ ->
      die $ usage name
