-- |
-- | A module that holds common methods.
-- |
-----------------------------------
module Common.Lib where

import           Data.Bits             (Bits (shiftL, (.|.)))
import           Data.Char             (ord)
import           Data.Foldable         (toList)
import           Data.Functor.Identity (Identity)

import           Data.Map              (Map)

import           Data.List             (group, sort)
import qualified Data.Map              as Map
import qualified Text.Parsec           as Parsec


getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

-- | Shortcut for parsing a string with Parsec
parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"

-- | maps indexed
mapIdx :: (a -> Int -> b) -> [a] -> [b]
mapIdx f l = zipWith f l [0..]

-- | parses a list like 1,2,3,4,5 to [1,2,3,4,5]
commaListParser :: String -> IO (Either Parsec.ParseError [Int])
commaListParser s = fmap (fmap read) . parse (Parsec.sepBy (Parsec.many Parsec.digit) (Parsec.char ',')) <$> readFile s

-- | Creates a bitmask out of the char using the ord function
-- | ord 'a' = 97
maskChar :: Integer -> Char -> Integer
maskChar acc c = acc .|. 1 `shiftL` (ord c - 97)

-- | Frequency Map
frequencyMap :: (Foldable f, Ord a) => f a -> Map a Int
frequencyMap = Map.fromListWith (+) . map (\x -> (head x, length x)) . group . sort . toList

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
