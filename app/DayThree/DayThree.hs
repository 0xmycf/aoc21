module DayThree.DayThree
( mainDayThree
, testDayThree
) where

import Common.Lib (getLines)
import Old.DayThreeSrc ( getGamma, getEpsilon, multGammaEps, getOxyOrCO2, hasMoreOrLess, getFirstChars )
import Prelude hiding (lines)

{-
    Input is 
        binary
        12 digits long

    Example
    1010
    1110
    0011
    1000

    Gamma   Value is the most  common bit for each index i \in [1,12] \subset N
    Epsilon Value is the least common bit for each index i \in [1,12] \subset N
    <=>
    Epsilon Value is Gamma Value flipped (1010 xor 1111 = 0101).

    General Idea
    1. Read input
    2. partition input depending on the ith character
    3. choose ith char of bigger list
    4. Repeat 2-4 until i==12
    5. Construct Epsilon Value by converting 1 to 0 or 0 to 1
    6. Parse binary expression to decimal
    7. return binary Gamma * binary Epsilon

    In the end I did not use any bitwise operators.
    When thinking about the problem I never found a way to go through the list only once...
    the splitting up stuff is also relatively easy to implement.

    I think my problem 1 solution is okish...
    I don't really like my problem 2 solution though... it seems very inefficient,
    there is also no correlation between part 1 and part 2 which is sad too,
    you should probably be able to reuse part 1 functions for part 2, which I didn't really do.

    I might come back to this in the future.
-}

inputPath :: String
inputPath = "./app/DayThree/input.txt"

mainDayThree :: IO ()
mainDayThree = putStrLn "Day Three..." >> problemOne >> problemTwo >> putStrLn "Day Three over."

testDayThree :: IO ()
testDayThree = do
    putStrLn "Test Day Three..."
    putStrLn "Tests for Day Three are in the test folder."
    putStrLn "Test Day Three over.\n"


problemOne :: IO ()
problemOne = do
    lines <- getLines inputPath
    let gamma = getGamma [] lines
    let eps   = getEpsilon gamma
    print $ multGammaEps gamma eps


{-
    Day Three Problem Two Plan
    1. Get Input
    2. Partition for i-th value
    3. Hold greater list, if equal hold list with 1
    4. if only one element in list return that element
    5. repeat 2-5 until you have the oxygen
    6. Repeat 2-5 with smaller list until you have CO2 value
    7. Convert both values to decimal and multiply
-}

problemTwo :: IO ()
problemTwo = do
    lines <- getLines inputPath
    let oxy = getOxyOrCO2 lines (getFirstChars '1' (hasMoreOrLess (<)))
    let co2 = getOxyOrCO2 lines (getFirstChars '0' (hasMoreOrLess (>)))
    print $ multGammaEps oxy co2

