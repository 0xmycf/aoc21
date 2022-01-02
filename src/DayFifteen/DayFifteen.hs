module DayFifteen.DayFifteen
( mainDayFifteen
, testDayFifteen
) where

import           Common.Graph (adjFromNeighbs)
import           Common.Lib   (Point, getLines, getNeighbors, gridParser)
import           Data.Char    (digitToInt)
import qualified Data.Heap    as Heap
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Maybe   (isJust)
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Linear       (V2 (V2))

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayFifteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayFifteen.txt"

data Knot = Knot {
              _val  :: Int
            , _succ :: Maybe Knot
            } deriving (Show, Read, Eq)

instance Ord Knot where
  Knot val _ `compare` Knot val2 _ = compare val val2

type Cave = Map Point Int
type Path = [Point]
type Nodes = Map Point Knot

-- | parses the input depending on the FilePath given
input :: FilePath -> IO Cave
input path = gridParser digitToInt <$> getLines path

mainDayFifteen :: IO ()
mainDayFifteen = putStrLn "Day Fifteen..." >> problemOne >> problemTwo >> putStrLn "Day Fifteen over.\n "

testDayFifteen :: IO ()
testDayFifteen = do
    putStrLn "Test Day Fifteen..."
    inp <- input testPath
    print . adjFromNeighbs (const True) $ inp
    print (Heap.fromList [Knot 100 Nothing , Knot 2 Nothing, Knot 10 (Just $ Knot 10000000 Nothing)] :: Heap.MinHeap Knot)
    writeFile "./inputs/test/parsedout/15.txt" (show inp)
    putStrLn "Test Day Fifteen over.\n"


problemOne :: IO ()
problemOne = print "to be impl"

problemTwo :: IO ()
problemTwo = print "to be impl"

endNode :: Point
endNode = V2 100 100

startNode :: Point
startNode = V2 1 1

-- | Dijkstra, but I didn't know it had a name
path :: Cave -> Path
path cave = go startNode Set.empty (Map.singleton startNode (Knot 0 Nothing))
    where
    go :: Point -> Set Point -> Nodes -> Path
    go currentCave seen nodes =
        let seen' = Set.insert currentCave seen
            nbs   = filter (\v -> let nb = Map.lookup v cave in isJust nb && v `notElem` seen') . getNeighbors $ currentCave
            --heap  = Heap.
        in  undefined



{-
    Path:
    1. What do we need to track?
        - Seen Nodes
        - Other paths
        - Current path
        - other path
        - end node ?
-}
