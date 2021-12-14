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

data Game = Win Int | Queue (Map Integer (Board BoardNumber))

instance Show Game where
  show (Win   a) = "Win " ++ show a
  show (Queue _) = "Queue"

-- | Might change later.
instance Eq Game where
  Win _   == Win _   = True
  Queue _ == Queue _ = True
  _       == _       = False

isQueue :: Game -> Bool
isQueue (Win   _) = False
isQueue (Queue _) = True

isWin :: Game -> Bool
isWin = not . isQueue

instance Show BoardNumber where
  show bn = show (number bn) ++ "/" ++ show (marked bn)

-- | This allows for quick concentration of the game points.
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
-- | { Bitmask => Board BoardNumber } 
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


-- I wonder if there is a way to write this less confusing...
-- | Takes in the Boards as well as the RNG Num List, returns a Win
drawAndMark :: Map Integer (Board BoardNumber) -> [Int] -> Game 
drawAndMark ms = mark (Queue ms)
  where
    mark :: Game -> [Int] -> Game
    mark g []             = if isQueue g then error "No winner!" else g
    mark (Win a) x        = Win a
    mark (Queue m) (x:xs) = do
        mark (Map.foldrWithKey (\ k _ g -> do
            case g of
                (Win a)     -> Win a                                    -- If it is a Win, short circuit <- This is monadic behavior, but I couldn't be bothered to look into that more... maybe some other time when I want to tidy this up a bit.
                (Queue map) -> if check x k then notify x k map else g) -- This part was very important, if I wouldn't notify map, I would not update all Maps!
            (Queue m) m) xs

-- | Updates the board and if its a Win returns a Win.
notify :: Int -> Integer -> Map Integer (Board BoardNumber) -> Game
notify i k ms = bMark (ms ! k)
  where
    bMark :: Board BoardNumber -> Game
    bMark b = do
      -- This part should get thoroughly tested
       let new = M.mapPos swapAndNotify b
       let rowCheck = any (\v ->  and . M.toList . fmap marked . M.submatrix v v 1 5 $ new) [1..5]
       let colCheck = any (\v ->  and . M.toList . fmap marked . M.submatrix 1 5 v v $ new) [1..5]
       if rowCheck || colCheck
       then Win (i * concentrate new)
       else Queue (Map.adjust (const new) k ms)
        where
          swapAndNotify _ a =
            if number a == i
            then Bn (number a) True
            else a
