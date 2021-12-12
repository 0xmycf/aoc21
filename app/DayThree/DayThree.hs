module DayThree.DayThree
( mainDayThree
, testDayThree
) where

import Lib (getLines)
import DayThreeSrc ( getGamma, getEpsilon, multGammaEps )

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
-}

inputPath :: String
inputPath = "./app/DayThree/input.txt"

mainDayThree :: IO ()
mainDayThree = putStrLn "Day Three..." >> problemOne >> problemTwo >> putStrLn "Day Three over."

testDayThree :: IO ()
testDayThree = do
    putStrLn "Test Day Three..."
    putStrLn "Test Day Three over.\n"



problemOne :: IO ()
problemOne = do
    lines <- getLines inputPath
    let gamma = getGamma [] lines
    let eps   = getEpsilon gamma
    print $ multGammaEps gamma eps


problemTwo :: IO ()
problemTwo = print "To Be implemented"

