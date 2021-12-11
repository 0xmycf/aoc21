module DayThree.DayThree 
( mainDayThree
, testDayThree
) where

import Lib (getLines)

inputPath :: String 
inputPath = "./app/DayThree/input.txt"

mainDayThree :: IO ()
mainDayThree = putStrLn "Day Three..." >> problemOne >> problemTwo >> putStrLn "Day Three over."

testDayThree :: IO ()
testDayThree = do
    putStrLn "Test Day Three..."
    lines <- getLines inputPath
    -- mapM_ print lines
    putStrLn "Test Day Three over.\n"



problemOne :: IO ()
problemOne = print "To Be implemented"


problemTwo :: IO ()
problemTwo = print "To Be implemented"