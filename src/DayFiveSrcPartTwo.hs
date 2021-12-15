{-# LANGUAGE FlexibleContexts #-}
module DayFiveSrcPartTwo 
( mv2ToInt
, getVertAndHoriz
, mapParseToPair
) where

import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT)
import Linear.V2 (V2 (V2))
import Data.Map (Map)

import qualified Text.Parsec     as Parsec
import qualified Data.Map.Strict as Map

{-
    Because my other solution is so slow and doesn't even work.
    I decided to start all over again.
    But I peaked into here:
    https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day04.hs
-}

data Direction = Horizontal | Vertical | Up | Down deriving (Show, Eq, Enum)

isVertOrHoriz :: Direction -> Bool
isVertOrHoriz d
    | d == Horizontal || d == Vertical = True
    | otherwise                        = False

isVertOrHorizL :: LineB a -> Bool
isVertOrHorizL (LineB _ _ d _)
    | d == Horizontal || d == Vertical = True
    | otherwise                        = False

-- | filters a list of Lines to Lines which are either going sideways or straight up.
getVertAndHoriz :: [LineB a] -> [LineB a]
getVertAndHoriz = filter isVertOrHorizL

data LineB a = LineB {
    _from, _to :: V2 a          -- fr,om -> t,o
  , _vh        :: Direction     -- 
  , _axis      :: a             -- y for Horizontal, x for Vertical
  }

instance (Show a) => Show (LineB a) where
  show (LineB (V2 f1 f2) (V2 t1 t2) d _) = show f1 ++ "," ++ show f2 ++ " -> " ++ show t1 ++ "," ++ show t2

-- | Parses one input Line, where one Line looks like:
-- | a,b -> c,d
parseToPair :: ParsecT String u Identity (LineB Int)
parseToPair = do
    pair1   <- pairing
    sep
    pair2   <- pairing
    let (dir,a) = whichDirection pair1 pair2
    return $ LineB {_from=pair1, _to=pair2, _vh=dir, _axis=a}
    where
        whichDirection (V2 a b) (V2 c d)
            | a == c         = (Vertical, a)
            | b == d         = (Horizontal, b)
            | (a+b) == (c+d) = (Up, 0)                              -- this might change later!
            | (a-b) == (c-d) = (Down, 0)
            | otherwise      = error "Couldn't match any direction"

-- | Parses a pair of numbers to a Haskell Int Pair
-- | Format: 123,21
pairing :: ParsecT String u Identity (V2 Int)
pairing = do
        num1 <- Parsec.many Parsec.digit
        Parsec.char ','
        num2 <- Parsec.many Parsec.digit
        return (V2 (read num1) (read num2) :: V2 Int)

-- | Parser for the input File
mapParseToPair :: ParsecT String u Identity [LineB Int]
mapParseToPair = Parsec.sepBy parseToPair (Parsec.char '\n')

-- | A separator parser, where a separator looks like:
-- | \^-+>$\ in Regex.
sep :: ParsecT String u Identity ()
sep = Parsec.spaces >> arrow >> Parsec.spaces
    where arrow = Parsec.many $ Parsec.char '-' >> Parsec.char '>'

-- | Creates a Map of V2 a a to Int based on the interceptions from a list of LineBs
mv2ToInt :: (Ord a, Enum a, Num a) => [LineB a] -> Map (V2 a) Int
mv2ToInt = mapping Map.empty
    where
        mapping acc []     = acc
        mapping acc (LineB (V2 f1 f2) (V2 t1 t2) b a:xs)
            | Vertical   == b = mapping (folding acc [V2 a' b' | let a' = a  , b' <- sorting f2 t2]) xs
            | Horizontal == b = mapping (folding acc [V2 b' a' | let a' = a  , b' <- sorting f1 t1]) xs
            | Up         == b = mapping (folding acc [V2 a' b' | a' <- sorting f1 t1, b' <- sorting f2 t2, a' + b' == f1 + f2]) xs
            | Down       == b = mapping (folding acc [V2 a' b' | a' <- sorting f1 t1, b' <- sorting f2 t2, a' - b' == f1 - f2]) xs
            | otherwise       = error "Error while creating the map"
            where
                folding acc' xs = foldr (\c p -> Map.insertWith (+) c 1 p) acc' xs

-- | takes in two numbers and constructs a list out if them.
sorting :: (Ord a, Enum a) => a -> a -> [a]
sorting a b = if a <= b then [a..b] else [b..a]
