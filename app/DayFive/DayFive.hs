module DayFive.DayFive
( mainDayFive
, testDayFive
) where

import DayFiveSrc (parse, mapParseToPair, getVhs)

import qualified Text.Parsec as Parsec

inputPath :: String
inputPath = "./app/DayFive/input.txt"

mainDayFive :: IO ()
mainDayFive = putStrLn "Day Five..." >> problemOne >> problemTwo >> putStrLn "Day Five over.\n "

testDayFive :: IO ()
testDayFive = do
    putStrLn "Test Day Five..."
    putStrLn "Test Day Five over.\n"


problemOne :: IO ()
problemOne = do
    lines <- readFile inputPath
    case parse mapParseToPair lines of
      Left  pe  -> print pe
      Right lis -> let vhs = getVhs lis in do 
          print $ "length lis: " ++ show (length lis) ++ " length Vhs: " ++ show (length vhs)
          
    print "foo"

problemTwo :: IO ()
problemTwo = print "to be impl"

