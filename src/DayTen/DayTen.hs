{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module DayTen.DayTen
( mainDayTen
, testDayTen
) where

import           Common.Lib (getLines)
import           Data.List  (sort)
import           Prelude    hiding (last)

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayTen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayTen.txt"

-- | parses the input depending on the FilePath given
input :: FilePath -> IO [String]
input = getLines

mainDayTen :: IO ()
mainDayTen = putStrLn "Day Ten..." >> problemOne >> problemTwo >> putStrLn "Day Ten over.\n "

testDayTen :: IO ()
testDayTen = do
    putStrLn "Test Day Ten..."
    inp <- input testPath
    let test = sum . fmap scoreCorrupted $ inp
    print $ "Test should be 26397"
    print $ "Test is: " ++ show test
    print $ "Result for test is: " ++ show (test == 26397)
    putStrLn "Test Day Ten over.\n"


problemOne :: IO ()
problemOne = input inputPath >>= print . sum . fmap scoreCorrupted

problemTwo :: IO ()
problemTwo = input inputPath >>= print . (\xs -> xs !! ((length xs - 1) `div` 2)) .  sort . filter (/=Just 0) . fmap (fmap scoringMissing <$> scoreMissing) . filter (not . filterCorrupted)

scoring :: Char -> Int
scoring ')' = 3
scoring ']' = 57
scoring '}' = 1197
scoring '>' = 25137
scoring  c  = error $ "This should never occur - scoring; char is: " ++ [c]

scoringMissing :: String -> Int
scoringMissing = foldl (\acc a -> acc * 5 + det a) 0
    where
    det ')' = 1
    det ']' = 2
    det '}' = 3
    det '>' = 4

close :: Char -> Char
close '<' = '>'
close '(' = ')'
close '{' = '}'
close '[' = ']'
close  _  =  error "This should never occur - close"

opened :: [Char]
opened = "<({["

scoreCorrupted :: String -> Int
scoreCorrupted []     = 0
scoreCorrupted (s:tr) = go [close s] tr
    where
    go _ [] = 0
    go [] _ = 0
    go pending@(p:dng) (x:xs)
        | x `elem` opened = go (close x : pending) xs
        | x == p          = go dng xs
        | x /= p          = scoring x

scoreMissing :: String -> Maybe String
scoreMissing []     = Nothing
scoreMissing (s:tr) = go [close s] [] tr
    where
    go p l [] = pure $ l ++ p
    go [] l _ = pure   l
    go pending@(p:dng) last (x:xs)
        | x `elem` opened = go (close x : pending) last xs
        | x == p          = go dng last xs
        | x /= p          = go dng (p : last) xs

-- | returns true if the string is corrupted, false otherwise
filterCorrupted :: String -> Bool
filterCorrupted []     = False
filterCorrupted (s:tr) = go [close s] tr
    where
    go _ [] = False
    go [] _ = False
    go pending@(p:dng) (x:xs)
        | x `elem` opened = go (close x : pending) xs
        | x == p          = go dng xs
        | x /= p          = True
