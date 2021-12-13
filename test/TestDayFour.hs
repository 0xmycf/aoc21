module TestDayFour
( testDayFourSuit
) where

import Lib (getLines)
import DayFourSrc
    ( prepare, mkBoard, inputBoards, parsedBoards, bToP )

inputPathSamples :: String
inputPathSamples = "test/samples/input3.txt"

testDayFourSuit :: IO ()
testDayFourSuit = do
    lines <- getLines inputPathSamples
    mapM_ print (inputBoards lines) -- (\l -> ',' `elem` l || l == ""
    -- print $ mkBoard . prepare .  map words . take 5 $ (inputBoards lines)
    print $ parsedBoards (inputBoards lines)
    print $ map mkBoard. parsedBoards $ inputBoards lines
    print $ bToP $ parsedBoards $ inputBoards lines
    -- print $ parsedBoards (inputBoards lines)

sample :: [[[Char]]]
sample = [
    ["22","13","17","11","0"],
    ["8","2","23","4","24"],
    ["21","9","14","16","7"],
    ["6","10","3","18","5"],
    ["1","12","20","15","19"]
    ]


prepared :: [Int]
prepared = prepare sample

