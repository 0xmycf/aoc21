module DaySixteen.DaySixteen
( mainDaySixteen
, testDaySixteen
) where

import           Common.Lib  (getLines, parse)
import qualified Text.Parsec as Parsec

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DaySixteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DaySixteen.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DaySixteen.txt"

-- | parses the input depending on the FilePath given
input :: FilePath -> IO ()
input path = let inputs = id <$> getLines path
             in pure ()

mainDaySixteen :: IO ()
mainDaySixteen = putStrLn "Day Sixteen..." >> problemOne >> problemTwo >> putStrLn "Day Sixteen over.\n "

testDaySixteen :: IO ()
testDaySixteen = do
    putStrLn "Test Day Sixteen..."
    putStrLn "Test Day Sixteen over.\n"


problemOne :: IO ()
problemOne = print "to be impl"

problemTwo :: IO ()
problemTwo = print "to be impl"
