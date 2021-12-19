module DayEight.DayEight
( mainDayEight
, testDayEight
) where

import Common.Lib (getLines, parse)
import qualified Text.Parsec as Parsec

inputPath :: String
inputPath = "./inputs/DayEight.txt"

testPath :: String
testPath  = "./inputs/test/DayEight.txt"

mainDayEight :: IO ()
mainDayEight = putStrLn "Day Eight..." >> problemOne >> problemTwo >> putStrLn "Day Eight over.\n "

testDayEight :: IO ()
testDayEight = do
    putStrLn "Test Day Eight..."
    putStrLn "Test Day Eight over.\n"


problemOne :: IO ()
problemOne = print "to be impl"

problemTwo :: IO ()
problemTwo = print "to be impl"