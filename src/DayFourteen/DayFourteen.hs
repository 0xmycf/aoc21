module DayFourteen.DayFourteen
( mainDayFourteen
, testDayFourteen
) where

import           Common.Lib  (getLines, parse)
import qualified Text.Parsec as Parsec

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayFourteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayFourteen.txt"

-- | parses the input depending on the FilePath given
input :: FilePath -> IO ()
input path = let inputs = id <$> getLines path
             in pure ()

mainDayFourteen :: IO ()
mainDayFourteen = putStrLn "Day Fourteen..." >> problemOne >> problemTwo >> putStrLn "Day Fourteen over.\n "

testDayFourteen :: IO ()
testDayFourteen = do
    putStrLn "Test Day Fourteen..."
    putStrLn "Test Day Fourteen over.\n"


problemOne :: IO ()
problemOne = print "to be impl"

problemTwo :: IO ()
problemTwo = print "to be impl"
