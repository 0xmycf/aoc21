module DaySeven.DaySeven
( mainDaySeven
, testDaySeven
) where

import Lib (getLines, parse, commaListParser)
import qualified Text.Parsec as Parsec
import Data.List (sort)

inputPath :: String
inputPath = "./inputs/DaySeven.txt"

mainDaySeven :: IO ()
mainDaySeven = putStrLn "Day Seven..." >> problemOne >> problemTwo >> putStrLn "Day Seven over.\n "

testDaySeven :: IO ()
testDaySeven = do
    putStrLn "Test Day Seven..."
    putStrLn "Test Day Seven over.\n"

problemOne :: IO ()
problemOne = commaListParser inputPath >>= print . fmap (foldr (\x acc -> abs (x-331)+acc) 0 . sort)
-- 331 comes from the the input list passed into (sort inputList !! (length inputList `div` 2)) (its the median)
-- I did this problem in ghci and don't want to make my solution 
-- uglier than it needs to be so I skip the part where I get the 331 out of the List.

problemTwo :: IO ()
problemTwo = commaListParser inputPath >>= print . fmap (foldr (\x acc -> abs(sum[1..(abs(x-479))])+acc) 0 .sort) 
-- 479 is the average of the (inputList + 1)
-- [1..(abs(x-479))] norms 479 to fit a range that has a fitting range for the position
-- then it yields the numbers 1+2+3... etc until the destination is reached.
-- The sum of this list is the amount of fuel needed.
-- I did this problem in ghci and don't want to make my solution 
-- uglier than it needs to be so I skip the part where I get the 331 out of the List.