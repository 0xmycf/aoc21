module TestsDayThree
( testDayThreeSuit
) where

import Data.List (partition)
import Control.Exception (assert)
import Old.DayThreeSrc (binToDec)

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
    print $ getFirstChars '1' (hasMoreOrLess (<)) sample 0 == sampleOnes
    oxy <- getOxyOrCO2 sample (getFirstChars '1' (hasMoreOrLess (<)))
    print $                  oxy == "10111"
    co2 <- getOxyOrCO2 sample (getFirstChars '0'(hasMoreOrLess (>)))
    print $ co2
    print $                  co2 == "01010"
    print $ multGammaEps co2 oxy
    print $ multGammaEps co2 oxy == 230
    putStrLn "Test Day ThreeSuit over."

getFirstChar :: [[Char]] -> Char
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

getFirstChars :: Char -> (([[Char]], [[Char]]) -> c) -> [[Char]] -> Int -> c
getFirstChars c f xs i = (f . partition (\v -> (v !! i) == c)) xs

-- | for hasMore pass in <, for hasLess pass in >
hasMoreOrLess :: Foldable t => (Int -> Int -> Bool) -> (t a, t a) -> t a
hasMoreOrLess f (xs, ys)
    | length xs `f` length ys       = ys
    | not (length xs `f` length ys) = xs
    | length xs == length ys        = xs -- xs will hold the 'c's in the beginning!

hasMoreOrLess _ (_, _) = error "Invalid input."

{-
    Day Three Plan Problem Three
    1. Get Input
    2. Partition for i-th value
    3. Hold greater list, if equal hold list with 1
    4. if only one element in list return that element
    5. repeat 2-5 until you have the oxygen
    6. Repeat 2-5 with smaller list until you have CO2 value
    7. Convert both values to decimal and multiply
-}

getOxyOrCO2 :: [String] -> ([[Char]] -> Int -> [[Char]]) -> IO String
getOxyOrCO2 [x] _ = return x
getOxyOrCO2 xs  f = oxyMoxy xs 0
    where
        oxyMoxy :: [String] -> Int -> IO String
        oxyMoxy [x] _ = return x
        oxyMoxy xs i  = do
            print $ xs
            print $ i
            oxyMoxy (f xs i) (i+1)
