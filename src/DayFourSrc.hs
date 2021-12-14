module DayFourSrc where

import Data.Matrix (Matrix)
import Data.Map (Map, (!))
import Data.Bits ((.|.), Bits (shiftL, (.&.)))
import qualified Data.Matrix as M
import qualified Data.Map as Map
import Data.Semigroup (sconcat)
import Data.List (foldl')

{-
  General Idea for Problem One
  1. Parse input and construct the following
    1.1 A Dictionary with keys of a bitmask of all numbers inside a Board to the Board
    1.2 A List which holds the numbers which will be drawn.
    With this I don't need to iterate over all BoardPositions
  2. fold/map through 1.2 and mark BoardPositions in 1.1
    Marking goes as follows
    2.1 Check Bitmask if drawn value is inside the dict, if yes then goto 2.2 else next mask and goto 2.1
    2.2 fmap through the matrix with a function that checks if the Pos at number is the value and then sets it to true
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

data BoardNumber = Bn {number :: Int, marked :: Bool} deriving (Eq)

data Game = Win Int | Queue deriving (Show)

instance Eq Game where
  Win _ == Win _ = True
  Queue == Queue = True
  _ == _         = False

instance Show BoardNumber where
  show bn = show (number bn) ++ "/" ++ show (marked bn)

instance Semigroup BoardNumber where
   (Bn a1 True)  <> (Bn b1 True)   = Bn 0 False
   (Bn a1 True)  <> (Bn b1 False)  = Bn b1 False
   (Bn a1 False) <> (Bn b1 True)   = Bn a1 False
   (Bn a1 False) <> (Bn b1 False)  = Bn (a1 + b1) False

concentrate :: Matrix BoardNumber -> Int
concentrate xs = number (foldl (<>) (Bn 0 True) . M.toList $ xs)

instance Ord BoardNumber where
  Bn a _ <= Bn b _ = a <= b

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


parseNums :: [String] -> [String]
parseNums = takeWhile (',' `elem`)

pToList :: [String] -> [Int]
pToList = map read . concatMap (words . map repl)
  where
    repl ',' = ' '
    repl c   = c

-- | Pass in 'Lines' go get the correct answer.
getParsedIntList :: [String] -> [Int]
getParsedIntList xs = pToList (parseNums xs)

-- | Pass in 'Lines' go get the correct answer.
getParsedIntListBoard :: [String] -> [[Int]]
getParsedIntListBoard = parsedBoards . inputBoards

-- | Pass in 'Lines' go get the correct answer.
getParsedBoards :: [String] -> [Board BoardNumber]
getParsedBoards = map mkBoard . getParsedIntListBoard

-- | maps the unparsed boards to a dictionary
-- | { Bitmask => ((Num => Pos), Board BoardNumber) } 
-- | It is important that the keys are Integers instead if Int's, otherwise we get arithmetic overflow errors!
bToP :: [[Int]] -> Map Integer (Board BoardNumber)
bToP xs = Map.fromList $ prepIntMap xs

prepIntMap :: (Bits a, Num a) => [[Int]] -> [(a, Board BoardNumber)]
prepIntMap xs = zip (cBitMask xs) (map mkBoard xs)


-- | creates a bitmask out of a [[Int]] List as we have it.
cBitMask :: (Foldable t, Bits a, Num a) => [t Int] -> [a]
cBitMask [] = []
cBitMask xs = map (foldr biting 0) xs
    where
        biting c p = p .|. 1 `shiftL` (c - 1)

-- | checks if given int is inside bitmask
check :: (Bits a, Num a) => Int -> a -> Bool
check n mask = 1 `shiftL` (n - 1) .&. mask == 1 `shiftL` (n - 1)

-- | Takes in the Boards as well as the RNG Num List
drawAndMark :: Map Integer (Board BoardNumber) -> [Int] -> Game -- Always returns Queue.. Which shouldn't happen...
drawAndMark ms = mark Queue
  where
    mark :: Game -> [Int] -> Game
    mark g []     = g
    mark (Win a) x= Win a
    mark _ (x:xs) = mark (Map.foldrWithKey (\ k _ g -> if (g == Queue) && check x k then notify x k ms else g) Queue ms) xs


-- | I don't know how to go through the Board, mark the correct fields AND return the correct result...
notify :: Int -> Integer -> Map Integer (Board BoardNumber) -> Game
notify i k ms = bMark (ms ! k)
  where
    bMark :: Board BoardNumber -> Game
    bMark b = do
      -- This part should get thoroughly tested
       let new = M.mapPos swapAndNotify b
       let rowCheck = all (\v ->  and . M.toList . fmap marked . M.submatrix v v 1 5 $ b) [1..5]
       let colCheck = all (\v ->  and . M.toList . fmap marked . M.submatrix 1 5 v v $ b) [1..5]
       if rowCheck || colCheck
       then Win (i * concentrate new)
       else Queue

      --  if marked (foldl (<>) (Bn 0 True) . M.toList $ new)
      --     -- this is always false ... I need to iterate through all rows/columns and see if all are marked
      --     -- If yes => then I need to call that method and retrieve the value and * with i.
      --  then Win 2
      --  else Queue
        where
          swapAndNotify (r,c) a = -- idk how to safe r and c to check ... I either just check every Row / Column or I safe it to a file ... which is very scuffed.
            if number a == i
            then Bn (number a) True
            else a

            -- if number a == i -- Bn (number a) True 
            -- then do
            --   let row = and . M.toList . fmap marked . M.submatrix r r 1 5 $ b
            --   if row
            --   then Win (2)             -- to to changed
            --   else Queue               -- to to changed
            -- else Queue

      -- let indices = [(a,b) | a <- [1..5], b <- [1..5]]
      -- goThroughBoard indices Queue
      --   where
      --     goThroughBoard :: [(Int, Int)] -> Game -> Game
      --     goThroughBoard []     acc = acc
      --     goThroughBoard (x:xs) acc = checkElem
      --       where
      --         checkElem :: Game
      --         checkElem =
      --           if number (b M.! x) == i
      --           then do
      --             goThroughBoard xs (Win 4)
      --           else
      --             goThroughBoard xs Queue



-- | Solution for Problem 1 Part 2...
{-
  Since I don't know how to effectively traverse the Matrix I had to change my initial plan...
  I will now store all Boards in Matrices which are stored in a List.
-}

-- type Board = Matrix

-- data BoardNumber = Bn {number :: Int, marked :: Bool} deriving (Eq)

-- data Game = Win Int | Queue deriving (Show)

-- instance Show BoardNumber where
--   show bn = show (number bn) ++ "/" ++ show (marked bn)

-- instance Semigroup BoardNumber where
--    (Bn a1 True)  <> (Bn b1 True)   = Bn 0 False
--    (Bn a1 True)  <> (Bn b1 False)  = Bn b1 False
--    (Bn a1 False) <> (Bn b1 True)   = Bn a1 False
--    (Bn a1 False) <> (Bn b1 False)  = Bn (a1 + b1) False

-- instance Ord BoardNumber where
--   Bn a _ <= Bn b _  = a <= b

-- toBoardNumber :: Int -> BoardNumber
-- toBoardNumber x = Bn x False

-- mkBoard :: [Int] -> Board BoardNumber
-- mkBoard [] = M.fromList 5 5 (replicate 25 (toBoardNumber 0))
-- mkBoard xs = M.fromList 5 5 (map toBoardNumber xs)

-- drawAndMark :: [Board BoardNumber] -> [Int] -> Int
-- drawAndMark bs = foldl' mark 0 -- (\a b -> b)
--   where
--     mark i j = M.foldl