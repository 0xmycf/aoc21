module DayFour.DayFour
( mainDayFour
, testDayFour
) where

import Common.Lib (getLines)
import Old.DayFourSrc (drawAndMark, getParsedBoards, getParsedIntList, drawAndMarkPart2, bToP, parsedBoards, inputBoards, pToList, parseNums)

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
problemTwo = do
    lines <- getLines inputPath
    let mapped = bToP $ parsedBoards $ inputBoards lines
    print $ drawAndMarkPart2 mapped (pToList (parseNums lines))