module INIMain where

import INI
import Data.Map
import System.Environment
import System.Directory
import Control.Monad
import Text.Trifecta

scanFiles :: FilePath -> IO [FilePath]
scanFiles fp = do
  ps <- fmap ((fp <> "/") <>) <$> listDirectory fp
  fs <- filterM doesFileExist ps
  dr <- filterM doesDirectoryExist ps
  fs' <- mconcat <$> forM dr scanFiles
  return $ fs ++ fs'

parseFile :: FilePath -> IO (Map FilePath Config)
parseFile fp = do
  s <- readFile fp
  return $ case parse s of
             (Success c) -> singleton fp c
             (Failure _) -> empty

main :: IO ()
main = do
  ds <- getArgs
  fs <- foldMap scanFiles ds
  cs <- traverse parseFile fs
  let mf = mconcat cs
  print $ keys mf
