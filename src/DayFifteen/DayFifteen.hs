module DayFifteen.DayFifteen
( mainDayFifteen
, testDayFifteen
) where

import           Common.Lib (Point, getLines, gridParser, getNeighbors)
import           Data.Char  (digitToInt)
import           Data.Map   (Map)
import           Data.Set   (Set)
import           Linear     (V2 (V2))
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import Data.Maybe (isJust)

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayFifteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayFifteen.txt"

type Cave = Map Point Int
type Path = [Point]

-- | parses the input depending on the FilePath given
input :: FilePath -> IO Cave
input path = gridParser digitToInt <$> getLines path

mainDayFifteen :: IO ()
mainDayFifteen = putStrLn "Day Fifteen..." >> problemOne >> problemTwo >> putStrLn "Day Fifteen over.\n "

testDayFifteen :: IO ()
testDayFifteen = do
    putStrLn "Test Day Fifteen..."
    inp <- input testPath
    writeFile "./inputs/test/parsedout/15.txt" (show inp)
    putStrLn "Test Day Fifteen over.\n"


problemOne :: IO ()
problemOne = print "to be impl"

problemTwo :: IO ()
problemTwo = print "to be impl"

endNode :: Point
endNode = V2 100 100

-- | Dijkstra, but I didn't know it had a name
path :: Cave -> Point -> Point -> Path 
path cave start endPos = go (Set.singleton start) Map.empty [] 0 start
    where
    -- | Set Point    = All already visited points
    -- | Map Path Int = All paths visited beforehand
    -- | Path         = The current path
    go :: Set Point -> Map Path Int -> Path -> Int -> Point -> Path
    go seen otherPaths currentPath currentDanger currentPos
        | any ( > currentDanger) (Map.elems otherPaths) = undefined 
        | currentPos == endPos = currentPath
        | otherwise = undefined
        where
        neighbs = filter (\v -> let nb = Map.lookup v cave in isJust nb && v `notElem` seen) . getNeighbors


{-
    Path:
    1. What do we need to track?
        - Seen Nodes
        - Other paths
        - Current path
        - other path
        - end node ?
-}
