module TestDayFive
( testDayFiveSuit

) where
import DayFiveSrc (mapParseToPair, parse)

testDayFiveSuit :: IO ()
testDayFiveSuit = do
    testTheInput

testTheInput :: IO ()
testTheInput = do
    file <- readFile "app/DayFive/input.txt"
    print $  parse mapParseToPair file