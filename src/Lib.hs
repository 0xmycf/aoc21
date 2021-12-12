module Lib where

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile