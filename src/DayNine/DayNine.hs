module DayNine.DayNine
( mainDayNine
, testDayNine
) where

import           Common.Lib (getLines, mapIdx)
import           Data.List  (sortBy, (\\))
import           Data.Map   (Map, toList)
import qualified Data.Map   as Map
import           Data.Maybe (isJust, mapMaybe)

type Coordinate = (Int, Int)

inputPath :: String
inputPath = "./inputs/DayNine.txt"

testPath :: String
testPath  = "./inputs/test/DayNine.txt"

input :: FilePath -> IO (Map Coordinate Int)
input path = let inputs = mapIdx (\a idx -> mapIdx (\b idx1 -> ((idx + 1, idx1 + 1), read [b])) a) <$> getLines path
            in do
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
    let lp = findLowestPointsAsList inp
    print $ lp
    print $ product . take 3 . sortBy (flip compare) . fmap (length . snd) . toList . field inp $ lp
    putStrLn "Test Day Nine over.\n"

problemOne :: IO ()
problemOne = do
    mp <- input inputPath
    -- Two ways to solve part 1
    print $ findLowestPoints mp
    print $ sum . fmap ((+1) . (mp Map.!)) . findLowestPointsAsList $ mp

problemTwo :: IO ()
problemTwo = do
    inp <- input inputPath
    print $ product . take 3 . sortBy (flip compare) . fmap (length . snd) . toList . field inp $ findLowestPointsAsList inp


-- | finds and sums the lowest points in the heightmap
--   Not optimized, just brute force
--   Not suitable for part 2
findLowestPoints :: Map Coordinate Int -> Int
findLowestPoints mp = Map.foldlWithKey comparing 0 mp
    where
        comparing :: Int -> Coordinate -> Int -> Int
        comparing acc (r,c) value
            -- corners
            | upLeft (r,c)
            && value < (mp Map.! (r+1,c))
            && value < (mp Map.! (r, c+1)) = acc + value + 1
            | upRight (r,c)
            && value < (mp Map.! (r,c-1))
            && value < (mp Map.! (r+1, c)) = acc + value + 1
            | btmLeft (r,c)
            && value < (mp Map.! (r-1,c))
            && value < (mp Map.! (r, c+1)) = acc + value + 1
            | btmRight (r,c)
            && value < (mp Map.! (r-1,c))
            && value < (mp Map.! (r, c-1)) = acc + value + 1
            -- sides
            | left (r,c) && not (upLeft (r,c)) && not (btmLeft (r,c))
            && value < (mp Map.! (r,c+1))
            && value < (mp Map.! (r+1,c))
            && value < (mp Map.! (r-1,c)) = acc + value + 1
            | right (r,c) && not (upRight (r,c)) && not (btmRight (r,c))
            && value < (mp Map.! (r+1,c))
            && value < (mp Map.! (r-1,c))
            && value < (mp Map.! (r,c-1)) = acc + value + 1
            | top (r,c) && not (upLeft (r,c)) && not (upRight (r,c))
            && value < (mp Map.! (r,c-1))
            && value < (mp Map.! (r, c+1))
            && value < (mp Map.! (r+1,c)) = acc + value + 1
            | btm (r,c) && not (btmLeft (r,c)) && not (btmRight (r,c))
            && value < (mp Map.! (r,c-1))
            && value < (mp Map.! (r, c+1))
            && value < (mp Map.! (r-1,c)) = acc + value + 1
            -- everything else
            | isElse (r,c)
            && value < (mp Map.! (r+1,c))
            && value < (mp Map.! (r,c+1))
            && value < (mp Map.! (r-1,c))
            && value < (mp Map.! (r,c-1)) = acc + value + 1
        comparing acc _  _ = acc


-- | finds the lowest points in the heightmap
findLowestPointsAsList :: Map Coordinate Int -> [Coordinate]
findLowestPointsAsList mp = Map.foldlWithKey finding [] mp
    where
        finding acc k value =
            let neighbs = mapMaybe (`Map.lookup` mp) $ getNeighbors k
            in if all (> value) neighbs
                then k : acc
                else acc


isCorner, upLeft, upRight, btmLeft, btmRight, edge, left, right, top, btm, isElse :: Coordinate -> Bool
isCorner c = c == (1,1) || c == (maxRow, maxCol) || c == (1, maxCol) || c == (maxRow, 1)
upLeft     = (==(1,1))
upRight    = (==(1,maxCol))
btmLeft    = (==(maxRow,1))
btmRight   = (==(maxRow,maxCol))
edge (r,c) = r == 1 || r == maxRow || c == 1 || c == maxCol
left (_,c) = c == 1
right(_,c) = c == maxCol
top  (r,_) = r == 1
btm  (r,_) = r == maxRow
isElse   c = not (isCorner c || edge c)

-- | Much more general, leave the boundary checking to the methods using this.
getNeighbors :: Coordinate -> [Coordinate]
getNeighbors (a,b) = [(a, b-1), (a+1, b), (a, b+1), (a-1, b)]


-- | Takes in the low points and creates a map with their corresponding fields
-- | including itself!
field :: Map Coordinate Int -> [Coordinate] -> Map Coordinate [Coordinate]
field mp = go Map.empty
    where
    go mp' []     = mp'
    go mp' (x:xs) = go (Map.insert x (getField x) mp') xs
    getField :: Coordinate -> [Coordinate]
    getField c = fielding [c] [] []
        where
        fielding :: [Coordinate] -> [Coordinate] -> [Coordinate] -> [Coordinate]
        fielding [] acc   _   = acc
        fielding (x:xs) acc vis =
            if x `elem` vis
            then fielding xs acc vis
            else let neighbs = filter (\v -> let p = v `Map.lookup` mp in isJust p && p /= Just 9) . getNeighbors $ x
                in fielding ((neighbs ++ xs) \\ vis) (x : acc) (x : vis)
