module DaySeventeen.DaySeventeen
( mainDaySeventeen
, testDaySeventeen
) where

import           Common.Lib    (getLines, parse)
import           Control.Arrow ((&&&))
import qualified Text.Parsec   as P

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DaySeventeen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DaySeventeen.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DaySeventeen.txt"

type Input = [String]

-- | parses the input depending on the FilePath given
input :: FilePath -> IO Input
input path = let inputs = id <$> getLines path
             in inputs

mainDaySeventeen :: IO ()
mainDaySeventeen = putStrLn "Day Seventeen..." >> input inputPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day Seventeen over.\n "

testDaySeventeen :: IO ()
testDaySeventeen = putStrLn "Day Seventeen..." >> input testPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day Seventeen over.\n "

problemOne :: Input -> String
problemOne = const "to be impl"

problemTwo :: Input -> String
problemTwo = const "to be impl"
