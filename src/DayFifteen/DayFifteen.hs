module DayFifteen.DayFifteen
( mainDayFifteen
, testDayFifteen
) where

import           Common.Lib    (Point, getLines, getNeighbors, gridParser)
import           Control.Monad ((>=>))
import           Data.Char     (digitToInt)
import           Data.Map      (Map)
import           Data.Maybe    (fromJust, fromMaybe, isJust)
import           Data.Set      (Set)
import           Linear        (V2 (V2))
import qualified Data.Heap     as Heap
import qualified Data.Map      as Map
import qualified Data.Set      as Set

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayFifteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayFifteen.txt"

data Knot = Knot {
              _val   :: Maybe Int
            , _point :: Point
            } deriving (Read)

instance Show Knot where
  show (Knot Nothing (V2 a b)) = "K " ++ "N" ++ " (" ++ show a ++ "," ++ show b ++ ")"
  show (Knot (Just val) (V2 a b)) = "K " ++ show val ++ " (" ++ show a ++ "," ++ show b ++ ")"

instance Eq Knot where
  Knot _ p1 == Knot _ p2 = p1 == p2

instance Ord Knot where
  Knot val _ `compare` Knot val2 _ = compare val val2

type Cave = Map Point Int
type Nodes = Map Point Knot

-- | parses the input depending on the FilePath given
input :: FilePath -> IO Cave
input = (gridParser digitToInt <$>) . getLines

mainDayFifteen :: IO ()
mainDayFifteen = putStrLn "Day Fifteen..." >> problemOne >> problemTwo >> putStrLn "Day Fifteen over.\n "

testDayFifteen :: IO ()
testDayFifteen = input testPath >>= path >>= (printReturn >=> writeFile "./inputs/test/parsedout/15.txt" . show) . Map.lookup (V2 10 10)

printReturn :: Show a => a -> IO a
printReturn a = do
    print a
    pure a

problemOne :: IO ()
problemOne = input inputPath >>= path >>= (printReturn >=> writeFile "./inputs/test/parsedout/15.txt" . show) . Map.lookup (V2 100 100)

problemTwo :: IO ()
problemTwo = print "Do you think I am insane or what? I cant run this code with an input 5 times as big..."

endNode :: Point
endNode = V2 100 100
--endNode = V2 10 10
--endNode = V2 3 3

startNode :: Point
startNode = V2 1 1

-- | (Bad) Dijkstra, but I didn't know it had a name
path :: Cave -> IO Nodes
path cave = go startNode Set.empty (Map.insert (V2 1 1) (Knot (Just 0) (V2 1 1)) $ Map.mapWithKey (const . Knot Nothing) cave) Heap.empty
    where
    go :: Point -> Set Point -> Nodes -> Heap.MinHeap Knot -> IO Nodes
    go _ _ nodes _ | all isJust (fmap (_val . snd) . Map.toList $ nodes) = pure nodes
    go currentCave _ nodes _ | currentCave == endNode = pure nodes
    go currentCave seen nodes heap = do
        let seen'   = Set.insert currentCave seen
            nbs     = filter (\v -> let nb = Map.lookup v cave in isJust nb && v `notElem` seen') . getNeighbors $ currentCave
            nbKnots = (toJust . (nodes Map.!) <$> nbs)
            heap'   = recalculateHeap (filter (\(Knot _ p) -> p `notElem` seen') (nbKnots ++ Heap.toList heap))
            nodes'  = Map.union (Map.fromList $ nbs `zip` nbKnots) nodes
            p       = _point . fromMaybe (error $ show (Heap.viewHead heap') ++ "  " ++ show currentCave ++ "  " ++ show nbs ) $ Heap.viewHead heap'
            in do
                -- its interesting seeing it go up and down
                print $ Heap.size heap'
                print $ "currentCave  " ++ show currentCave
                go p (Set.insert currentCave seen) nodes' heap'
        where
        Knot val _ = nodes Map.! currentCave
        toJust (Knot Nothing a) = Knot (fmap (+( cave Map.! a   )) val) a
        toJust k@(Knot (Just i) a)
            | i > (+pathDanger) (fromJust val) = Knot (fmap (+(cave Map.! a)) val) a
            | otherwise               = k
            where
            pathDanger = cave Map.! a


recalculateHeap :: [Knot] -> Heap.MinHeap Knot
recalculateHeap = Heap.fromList
