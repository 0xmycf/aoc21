module DayFourSrc where

import Data.Matrix (Matrix)
import Data.Map (Map)
import Data.Bits ((.|.), Bits (shiftL, (.&.)))
import qualified Data.Matrix as M
import qualified Data.Map as Map
import GHC.Num (minusInteger)

{-
  Nope doing the thing again Only happens with IntMaps!->| I tried a different approach (iterating through the whole matrix, because arithmetic overflow)
  General Idea for Problem One
  1. Parse input and construct the following
    1.1 A Dictionary with keys of a bitmask of all numbers inside a Board to (the Board, 1.1.1)
      1.1.1 A Dictionary with keys of the numbers inside the Board to their position inside the Board
            This allows for quick access.
    1.2 A List which holds the numbers which will be drawn.
    With this I don't need to iterate over all BoardPositions
  2. fold/map through 1.2 and mark BoardPositions in 1.1
    Marking goes as follows
    2.1 Check Bitmask if drawn value is inside the dict, if yes then goto 2.2 else next mask and goto 2.1
    2.2 Get the position for the number from 1.1.1 and mark it inside 1.1 (fst)
    2.3 If any value was marked, notify the row and the column in which the number resides, do as follows
      2.3.1 Extract the row and the column
      2.3.2 Iterate through both and if one is not marked then next number and goto 2. else goto 3
  3. If a win happened do as follows:
    3.1 Iterate through the board and filter all all BoardPositions which are not marked
    3.2 Calculate sum of 3.1
    3.3 Multiply 3.2 by the last drawn number then goto 4
  4. Return 3.3
-}

type Board = Matrix

data BoardNumber = Bn {number :: Int, marked :: Bool}

data Pos = Pos {r, c :: Int}

instance Show BoardNumber where
  show bn = show (number bn) ++ "/" ++ show (marked bn)

toBoardNumber :: Int -> BoardNumber
toBoardNumber x = Bn x False

mkBoard :: [Int] -> Board BoardNumber
mkBoard [] = M.fromList 5 5 (replicate 25 (toBoardNumber 0))
mkBoard xs = M.fromList 5 5 (map toBoardNumber xs)

-- | Parses the input which will be of type [[String]] <=> [[[Char]]]
-- | to a neat list of Integers which I can feed to the matrix.
prepare :: [[String]] -> [Int]
prepare = map read . concat

inputBoards :: [String] -> [String]
inputBoards = filter (/="") . dropWhile (',' `elem`) -- (\l -> length l /= 14)


{-
    What to do with parsedBoards?
    1. pass in the RAW input from inputBoards
    2. call words
    3. call prepare
    4. put output of prepare into a list
    5. return that list.
-}

-- | Should return a list of the different boards 
-- | BEFORE those are turned into actual Boards.
parsedBoards :: [String] -> [[Int]]
parsedBoards xs = map (prepare . map words) . group5 [] $ xs
  where
    group5 acc []             = acc
    group5 acc (a:b:c:d:e:xs) = group5 ([a,b,c,d,e] : acc) xs
    group5 _   xs             = error $ "Error while parsing " ++ show xs


-- | maps the unparsed boards to a dictionary
-- | { Bitmask => ((Num => Pos), Board BoardNumber) } 
-- bToP :: [[Int]] -> IntMap (Board BoardNumber)
bToP :: [[Integer]] -> Map Integer (Board BoardNumber)
bToP xs = Map.fromList $ prepIntMap xs 

-- prepIntMap :: (Bits a, Num a) => [[Integer]] -> [(a, Board BoardNumber)]
prepIntMap :: [[Integer]] -> [(Integer, Board BoardNumber)]
prepIntMap xs = zip (cBitMask xs) (map mkBoard xs)


-- | creates a bitmask out of a [[Int]] List as we have it.
cBitMask :: (Foldable t, Bits a, Num a) => [t Integer] -> [a]
cBitMask [] = []
cBitMask xs = map (foldr biting 0) xs
    where
        biting c p = p .|. (1 `shiftL` (c - 1))

-- | checks if given int is inside bitmask
check :: Integer -> Integer -> Bool
check n mask = (1 `shiftL` (n `minusInteger` 1)) .&. mask == (1 `shiftL` (n - 1))

createIM :: Board BoardNumber -> Map Int Int
createIM b = error "to be impl"