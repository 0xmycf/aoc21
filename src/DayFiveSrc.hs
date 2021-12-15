{-# LANGUAGE FlexibleContexts #-}
module DayFiveSrc
( mapParseToPair
, parse
, getVhs
) where

import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT)
import qualified Text.Parsec as Parsec
import Data.List (sort)

-- | Holds the point where the line starts to its endpoint.
-- | It also contains a Boolean which indicates if the line will be horizontal or vertical.
data Line a = Line {_from, _to :: (a,a), _vh :: Bool}

instance Show a => Show (Line a) where
  show l = show (_from l) ++ " -> " ++ show (_to l)

isVh :: (Num a, Eq a) => (a,a) -> (a,a) -> Bool
isVh (a1,a2) (b1,b2) = a1 == b1 || a2 == b2

getVhs :: [Line a] -> [Line a]
getVhs = filter _vh

-- | Shortcut for parsing a string with Parsec
parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"

-- | Parses one input Line, where one Line looks like:
-- | a,b -> c,d
parseToPair :: ParsecT String u Identity (Line Int)
parseToPair = do
    pair1 <- pairing
    sep
    pair2 <- pairing
    return $ Line {_from=pair1, _to=pair2, _vh=isVh pair1 pair2}
    where pairing = do
            num1 <- Parsec.many Parsec.digit
            Parsec.char ','
            num2 <- Parsec.many Parsec.digit
            return ((read num1, read num2) :: (Int, Int))

-- | Parser for the input File
mapParseToPair :: ParsecT String u Identity [Line Int]
mapParseToPair = Parsec.sepBy parseToPair (Parsec.char '\n')

-- | A separator parser, where a separator looks like:
-- | \^-+>$\ in Regex.
sep :: ParsecT String u Identity ()
sep = Parsec.spaces >> arrow >> Parsec.spaces
    where arrow = Parsec.many $ Parsec.char '-' >> Parsec.char '>'

-- | Generates the actual Line from starting point to finish
-- | Assumes that diagonals will lay on diagonals like 
-- | [(1,4),(2,3),(3,2),(4,1)] or [(1,1),(2,2),(3,3),(4,4)]
mkLine :: (Num a, Enum a, Ord a) => Line a -> [(a,a)]
mkLine (Line f t b) = if b then vhline else diagline
    where
        vhline   = [(a,b) |
            a <- sorting (fst f) (fst t),
            b <- sorting (snd f) (snd t)]
        diagline = 
            if uncurry (/=) f
            then
                [(a, b) | a <- uncurry sorting f, b <- uncurry sorting t, (b + a) == uncurry (+) f]
            else
                [(a, a) | a <- comparing]
            where sorting a b = if a <= b then [a..b] else [b..a]
                  comparing   = if fst f <= fst t then [(fst f)..(fst t)] else [(fst t)..(fst f)]
        sorting a b = if a <= b then [a..b] else [b..a]
