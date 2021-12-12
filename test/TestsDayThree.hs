{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module TestsDayThree
( testDayThreeSuit
) where

import Data.List (partition)
import Control.Exception (assert)
import DayThreeSrc (binToDec)

sample :: [String]
sample = [
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
    ]

sampleOnes :: [[Char]]
sampleOnes = fst $ partition (\v -> head v == '1') sample

sampleZeros :: [[Char]]
sampleZeros = fst $ partition (\v -> head v == '0') sample

answerGamma :: String
answerGamma = "10110"

answerEpsilon :: String
answerEpsilon = "01001" -- <=> answerGamma `bitwise xor` "11111"

testDayThreeSuit :: IO ()
testDayThreeSuit = do
    putStrLn "Test Day ThreeSuit..."
    print $ getFirstChar sample == '1'
    ggs <- getGamma [] sample
    print $ ggs == answerGamma
    print $ getEpsilon ggs == answerEpsilon
    print $ "binToDec Tests..."
    print $ binToDec "10101" == 21
    print $ binToDec ggs
    print $ binToDec "10110"
    print $ binToDec (getEpsilon ggs)
    print $ binToDec "01001"
    print $ binToDec "01001" == 9
    print $ "multGammaEps Tests..."
    print $ multGammaEps ggs (getEpsilon ggs)
    print $ multGammaEps ggs (getEpsilon ggs) == 198
    print $ "Problem 2"
    print $ getFirstChars sample == sampleOnes
    oxy <- getOxygen sample
    print $                  oxy == "10111"
    putStrLn "Test Day ThreeSuit over."

getFirstChar = isGreater . partition (\v -> head v == '1')
    where
        isGreater (xs, ys)
            | length xs < length ys     = head $ head ys
            | length xs > length ys     = head $ head xs
            | length xs == length ys    = '1'
        isGreater (_, _) = error "Invalid input."

getGamma :: [Char] -> [[Char]] -> IO [Char]
getGamma acc ("":_) = return $ reverse acc
getGamma acc xs     = do
    -- print $ "first char: " ++ [getFirstChar xs]
    -- print $ "drop 1: "
    -- print $ reducing xs
    getGamma (getFirstChar xs : acc) (reducing xs)

reducing :: [[a]] -> [[a]]
reducing = map (drop 1)

getEpsilon :: [Char] -> [Char]
getEpsilon = map flip
    where
        flip '1' = '0'
        flip  _  = '1'

multGammaEps :: String -> String -> Int
multGammaEps xs ys = binToDec xs * binToDec ys

-- | Part 2

getFirstChars :: [[Char]] -> [[Char]]
getFirstChars = hasMore . partition (\v -> head v == '1')
    where
        hasMore (xs, ys)
            | length xs < length ys     = ys
            | length xs > length ys     = xs
            | length xs == length ys    = xs -- xs will hold the '1's in the beginning!
        hasMore (_, _) = error "Invalid input."

{-
    Day Two Plan
    1. Get Input
    2. Partition for i-th value
    3. Hold greater list, if equal hold list with 1
    4. if only one element in list return that element
    5. repeat 2-5 until you have the oxygen
    6. Repeat 2-5 with smaller list until you have CO2 value
    7. Convert both values to decimal and multiply
-}

getOxygen :: [String] -> IO String
getOxygen _   = error "Inf Loop" -- Remove Overlapping Pattern thingy from the top!
getOxygen [x] = return x
getOxygen xs  = oxyMoxy xs []
    where
        oxyMoxy xs acc = do
            print $ reducing xs
            print $ getFirstChars (reducing xs)
            print $ getFirstChars xs
            oxyMoxy (getFirstChars (reducing xs)) []
