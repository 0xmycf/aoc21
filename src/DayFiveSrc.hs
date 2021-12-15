{-# LANGUAGE FlexibleContexts #-}
module DayFiveSrc
( mapParseToPair
, parse
, getVhs
, intersect
, intersectBool
, intersectP
, mkLine
) where

import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT)
import qualified Text.Parsec as Parsec
import Data.List (sort)
import qualified Data.Set as Set

-- | Holds the point where the line starts to its endpoint.
-- | It also contains a Boolean which indicates if the line will be horizontal or vertical.
data Line a = Line {_from, _to :: (a,a), _vh :: Bool}

instance (Eq a, Num a, Enum a, Ord a) => Eq (Line a) where
  (Line _ _ False) == (Line _ _ True)  = False
  (Line _ _ True)  == (Line _ _ False) = False
  (Line t1 f1 _)   == (Line t2 f2 _)   = f1 == f2 && t1 == t2 || f1 == t2 && f2 == t1

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
    
-- | Parses a pair of numbers to a Haskell Int Pair
-- | Format: 123,21
pairing :: ParsecT String u Identity (Int, Int)
pairing = do
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
            then if fst f /= snd t
                then
                    [(a, b) | a <- sorting (maxM t) (maxM f), let b = a - uncurry (-) f]
                else
                    [(a, b) | a <- uncurry sorting f, b <- uncurry sorting t, (b + a) == uncurry (+) f]
            else
                [(a, a) | a <- comparing]
            where sorting a b = if a <= b then [a..b] else [b..a]
                  comparing   = if fst f <= fst t then [(fst f)..(fst t)] else [(fst t)..(fst f)]

sorting :: (Ord a, Enum a) => a -> a -> [a]
sorting a b = if a <= b then [a..b] else [b..a]

intersect ::(Num a, Enum a, Ord a) => Line a -> Line a -> Int
intersect xs ys = length . Set.fromList $ [x | x <- mkLine xs ++ mkLine ys, x `elem` mkLine xs && x `elem` mkLine ys]

-- | Instead of the count of intersecting points it gives the points
intersectP :: (Ord a, Num a, Enum a) => Line a -> Line a -> Set.Set (a, a)
intersectP xs ys = Set.fromList $ [x | x <- mkLine xs ++ mkLine ys, x `elem` mkLine xs && x `elem` mkLine ys]

-- | Returns True if there are any intersections, False otherwise.
intersectBool ::(Num a, Enum a, Ord a) => Line a -> Line a -> Bool
intersectBool xs ys = not (null (Set.fromList [x | x <- mkLine xs ++ mkLine ys, x `elem` mkLine xs && x `elem` mkLine ys]))

test :: (Ord b, Enum b, Num b) => (b, b) -> (b, b) -> [(b, b)]
test f t = [(a, b) | a <- sorting (maxM t) (maxM f), let b = a - uncurry (-) f] -- Error on "5,5 -> 8,2"

maxM :: Ord a => (a, a) -> a
maxM (a,b) 
    | a <= b    = b
    | a > b     = a
    | otherwise = a

minM :: Ord a => (a, a) -> a
minM (a,b) 
    | a <= b    = a
    | a > b     = b
    | otherwise = a
