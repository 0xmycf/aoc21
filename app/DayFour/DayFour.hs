module DayFour.DayFour
( mainDayFour
, testDayFour
) where

import Lib (getLines)

inputPath :: String
inputPath = "./app/DayFour/input.txt"

mainDayFour :: IO ()
mainDayFour = putStrLn "Day Four..." >> problemOne >> problemTwo >> putStrLn "Day Four over.\n"

testDayFour :: IO ()
testDayFour = do
    putStrLn "Test Day Four..."
    putStrLn "Test Day Four over.\n "


problemOne :: IO ()
problemOne = do
    lines <- getLines inputPath
    mapM_ print lines

problemTwo :: IO ()
problemTwo = print "to be impl"