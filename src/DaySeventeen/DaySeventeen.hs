module DaySeventeen.DaySeventeen
( mainDaySeventeen
, testDaySeventeen
) where

import           Common.Lib  (getLines, parse)
import qualified Text.Parsec as Parsec

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DaySeventeen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DaySeventeen.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DaySeventeen.txt"

-- | parses the input depending on the FilePath given
input :: FilePath -> IO ()
input path = let inputs = id <$> getLines path
             in pure ()

mainDaySeventeen :: IO ()
mainDaySeventeen = putStrLn "Day Seventeen..." >> problemOne >> problemTwo >> putStrLn "Day Seventeen over.\n "

testDaySeventeen :: IO ()
testDaySeventeen = do
    putStrLn "Test Day Seventeen..."
    putStrLn "Test Day Seventeen over.\n"


problemOne :: IO ()
problemOne = print "to be impl"

problemTwo :: IO ()
problemTwo = print "to be impl"
