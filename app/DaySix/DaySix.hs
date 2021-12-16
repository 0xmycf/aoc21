module DaySix.DaySix
( mainDaySix
, testDaySix
) where

import qualified Text.Parsec as Parsec

import Lib (getLines, parse)

inputPath :: String
inputPath = "./app/DaySix/input.txt"

mainDaySix :: IO ()
mainDaySix = putStrLn "Day Six..." >> problemOne >> problemTwo >> putStrLn "Day Six over.\n "

testDaySix :: IO ()
testDaySix = do
    putStrLn "Test Day Six..."
    putStrLn "Test Day Six over.\n"


problemOne :: IO ()
problemOne = inputParser >>= print

problemTwo :: IO ()
problemTwo = print "to be impl"

inputParser :: IO (Either Parsec.ParseError [Int])
inputParser = fmap (fmap read) . parse (Parsec.sepBy (Parsec.many Parsec.digit) (Parsec.char ',')) <$> readFile inputPath