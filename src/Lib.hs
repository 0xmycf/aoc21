module Lib
  ( someFunc
  , getLines
  )
where

someFunc :: IO ()
someFunc = putStrLn "Printing SomeFunc imported from Lib"

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile