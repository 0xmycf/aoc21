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
