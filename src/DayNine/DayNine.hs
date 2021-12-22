module DayNine.DayNine
( mainDayNine
, testDayNine
) where

import           Common.Lib (getLines, mapIdx, parse)

import           Data.Map   (Map)
import qualified Data.Map   as Map

type Coordinate = (Int, Int)

inputPath :: String
inputPath = "./inputs/DayNine.txt"

testPath :: String
testPath  = "./inputs/test/DayNine.txt"

input :: FilePath -> IO (Map Coordinate Char)
input path = let inputs = mapIdx (\a idx -> mapIdx (\b idx1 -> ((idx + 1, idx1 + 1), b)) a) <$> getLines path
            in do
            x <- length . head <$> inputs
            y <- length <$> inputs
            cl <- concat <$> inputs
            pure $ Map.fromList cl

-- This would be the correct approach, however they are 100 100
-- maxCol, maxRow :: IO Int
-- maxCol = let inputs = getLines inputPath in length . head <$> inputs
-- maxRow = let inputs = getLines inputPath in length        <$> inputs

maxCol, maxRow :: Int
maxCol = 100
maxRow = 100

mainDayNine :: IO ()
mainDayNine = putStrLn "Day Nine..." >> problemOne >> problemTwo >> putStrLn "Day Nine over.\n "

testDayNine :: IO ()
testDayNine = do
    putStrLn "Test Day Nine..."
    inp <- input testPath
    print $ findLowestPoints inp
    putStrLn "Test Day Nine over.\n"


problemOne :: IO ()
problemOne = do
    mp <- input inputPath
    print $ findLowestPoints mp

problemTwo :: IO ()
problemTwo = print "to be impl"

-- | finds and sums the lowest points in the heightmap
--   Not optimized, just brute force
findLowestPoints :: Map Coordinate Char -> Int
findLowestPoints mp = Map.foldlWithKey comparing 0 mp
    where
        comparing :: Int -> Coordinate -> Char -> Int
        comparing acc (r,c) value
            -- corners
            | upLeft (r,c)
            && value < (mp Map.! (r+1,c))
            && value < (mp Map.! (r, c+1)) = acc + (read [value] + 1)
            | upRight (r,c)
            && value < (mp Map.! (r,c-1))
            && value < (mp Map.! (r+1, c)) = acc + (read [value] + 1)
            | btmLeft (r,c)
            && value < (mp Map.! (r-1,c))
            && value < (mp Map.! (r, c+1)) = acc + (read [value] + 1)
            | btmRight (r,c)
            && value < (mp Map.! (r-1,c))
            && value < (mp Map.! (r, c-1)) = acc + (read [value] + 1)
            -- sides
            | left (r,c) && not (upLeft (r,c)) && not (btmLeft (r,c))
            && value < (mp Map.! (r,c+1))
            && value < (mp Map.! (r+1,c))
            && value < (mp Map.! (r-1,c)) = acc + (read [value] + 1)
            | right (r,c) && not (upRight (r,c)) && not (btmRight (r,c))
            && value < (mp Map.! (r+1,c))
            && value < (mp Map.! (r-1,c))
            && value < (mp Map.! (r,c-1)) = acc + (read [value] + 1)
            | top (r,c) && not (upLeft (r,c)) && not (upRight (r,c))
            && value < (mp Map.! (r,c-1))
            && value < (mp Map.! (r, c+1))
            && value < (mp Map.! (r+1,c)) = acc + (read [value] + 1)
            | btm (r,c) && not (btmLeft (r,c)) && not (btmRight (r,c))
            && value < (mp Map.! (r,c-1))
            && value < (mp Map.! (r, c+1))
            && value < (mp Map.! (r-1,c)) = acc + (read [value] + 1)
            -- everything else
            | isElse (r,c)
            && value < (mp Map.! (r+1,c))
            && value < (mp Map.! (r,c+1))
            && value < (mp Map.! (r-1,c))
            && value < (mp Map.! (r,c-1)) = acc + (read [value] + 1)
        comparing acc cord value = acc

isCorner, upLeft, upRight, btmLeft, btmRight, edge, left, right, top, btm, isElse:: Coordinate -> Bool
isCorner c = c == (1,1) || c == (maxRow, maxCol) || c == (1, maxCol) || c == (maxRow, 1)
upLeft     = (==(1,1))
upRight    = (==(1,maxCol))
btmLeft    = (==(maxRow,1))
btmRight   = (==(maxRow,maxCol))
edge (r,c) = r == 1 || r == maxRow || c == 1 || c == maxCol
left (r,c) = c == 1
right(r,c) = c == maxCol
top  (r,c) = r == 1
btm  (r,c) = r == maxRow
isElse   c = not (isCorner c || edge c) 