module DayThreeSrc where
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
binToDec xs = go 0 (reverse xs)
    where
        go _ []     = 0
        go i (x:xs) = (2^i * read [x]) + go (i+1) xs
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