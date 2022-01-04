module DaySixteen.DaySixteen
( mainDaySixteen
, testDaySixteen
) where

import           Common.Lib (getLines)
import           Data.Map   (Map, lookup)
import qualified Data.Map   as Map
import           Data.Maybe (mapMaybe)
import           Prelude    hiding (lookup)

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DaySixteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DaySixteen.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DaySixteen.txt"

mapping :: IO (Map Char String)
mapping = do
    inp <- getLines "inputs/DaySixteenMapping.txt"
    pure $ Map.fromList $
        fmap toTuple inp
    where
    toTuple :: String -> (Char, String)
    toTuple (a:xs) = (a, xs)
    toTuple _      = ('Z', "")

-- | parses the input depending on the FilePath given
input :: FilePath -> IO String
input path = mapping >>= \x -> concat . mapMaybe (`lookup` x) <$> readFile path

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
