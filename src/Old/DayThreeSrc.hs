{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Old.DayThreeSrc where
import Data.List (partition)

-- | Converts a binary string to a decimal integer
-- | Example: 10101
-- | 2^0 * 1 + 2^1 * 0 + 2^2 * 1 + 2^3 * 0 + 2^4 * 1 = 1 + 4 + 16 = 21
-- | sum_{i=0}{j-1}(2^i * x)
-- |  where 
-- |   j is the length bitstring
-- |   x is the ith digit of the bitstring
-- | Figured the correct syntax with a bit of help from here:
-- | https://stackoverflow.com/questions/44217310/convert-binary-string-to-integer-value-using-first-order-functions
binToDec :: String -> Int
binToDec xs = go (0 :: Integer) (reverse xs)
    where
        go _ []      = 0
        go i (x:xs') = (2^i * read [x]) + go (i+1) xs'
{-
    This explains why we use the reverse at the beginning.

    Input "01001"
    go 0 "01001"
    1. (2^0 * 0) + go 1 "1001"
    2. (2^1 * 1) + go 2 "001"
    3. (2^2 * 0) + go 3 "01"
    4. (2^3 * 0) + go 4 "1"
    5. (2^4 * 1) + go 5 ""
    6. 0

    7. (2^0 * 0) + (2^1 * 1) + (2^2 * 0) + (2^3 * 0) + (2^4 * 1) + 0 =
    8.  0        +  2        +  0        +  0        +  16       + 0 = 18 != 9 = [01001]b2
-}

getGamma :: [Char] -> [[Char]] -> [Char]
getGamma acc ("":_) = reverse acc
getGamma acc xs     = getGamma (getFirstChar xs : acc) (reducing xs)
    where
        reducing xs = map (drop 1) xs

getEpsilon :: [Char] -> [Char]
getEpsilon = map flip
    where
        flip '1' = '0'
        flip  _  = '1'

getFirstChar :: [[Char]] -> Char
getFirstChar = isGreater . partition (\v -> head v == '1')
    where
        isGreater (xs, ys)
            | length xs < length ys     = head $ head ys
            | length xs > length ys     = head $ head xs
            | length xs == length ys    = '1'
        isGreater (_, _) = error "Invalid input."

multGammaEps :: String -> String -> Int
multGammaEps xs ys = binToDec xs * binToDec ys  

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

getFirstChars :: Char -> (([[Char]], [[Char]]) -> c) -> [[Char]] -> Int -> c
getFirstChars c f xs i = (f . partition (\v -> (v !! i) == c)) xs

-- | for hasMore pass in <, for hasLess pass in >
hasMoreOrLess :: Foldable t => (Int -> Int -> Bool) -> (t a, t a) -> t a
hasMoreOrLess f (xs, ys)
    | length xs `f` length ys       = ys
    | not (length xs `f` length ys) = xs
    | length xs == length ys        = xs -- xs will hold the '1's in the beginning!

hasMoreOrLess _ (_, _) = error "Invalid input."

getOxyOrCO2 :: [String] -> ([[Char]] -> Int -> [[Char]]) -> String
getOxyOrCO2 [x] _ = x
getOxyOrCO2 xs  f = oxyMoxy xs 0
    where
        oxyMoxy :: [String] -> Int -> String
        oxyMoxy [x] _ = x
        oxyMoxy xs i  = oxyMoxy (f xs i) (i+1)
