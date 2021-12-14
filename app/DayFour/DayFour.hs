module DayFour.DayFour
( mainDayFour
, testDayFour
) where

import Lib (getLines)
import DayFourSrc (drawAndMark, getParsedBoards, getParsedIntList, bToP, parsedBoards, inputBoards, pToList, parseNums)

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
    let mapped = bToP $ parsedBoards $ inputBoards lines
    print $ drawAndMark mapped (pToList (parseNums lines))

problemTwo :: IO ()
problemTwo = print "to be impl"