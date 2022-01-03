module DayFifteen.DayFifteen
( mainDayFifteen
, testDayFifteen
, problemOne
, problemTwo
) where

import           Common.Lib    (Point, getLines, getNeighbors, gridParser)
import           Control.Monad ((>=>))
import           Data.Char     (digitToInt, intToDigit)
import           Data.Function ((&))
import           Data.Functor  (($>))
import           Data.Map      (Map)
import           Data.Maybe    (fromJust, isJust, mapMaybe)
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

getActualCave :: FilePath -> IO Cave
getActualCave = (gridParser digitToInt . makeBigger <$>) . getLines
    where
    makeBigger :: [String] -> [String]
    makeBigger xs = fmap (concat . take 5 . iterate nextSection) xs ++ (concat . take 4 . fmap (goDown xs)) [1..]
    goDown :: [String] -> Int -> [String]
    goDown x d = let xs = fmap (concat . take 5 . drop d . iterate nextSection) x
               in xs

nextSection :: String -> String
nextSection = fmap nextNum

nextNum :: Char -> Char
nextNum '9' = '1'
nextNum  c  = (digitToInt c :: Int) + 1 & intToDigit

mainDayFifteen :: IO ()
mainDayFifteen = putStrLn "Day Fifteen..." >> problemOne >> problemTwo >> putStrLn "Day Fifteen over.\n "

testDayFifteen :: IO ()
testDayFifteen = getActualCave testPath >>= (printReturn >=> writeFile "./inputs/test/parsedout/15t.txt" . show) . path (V2 50 50)
--testDayFifteen = input testPath >>= (printReturn >=> writeFile "./inputs/test/parsedout/15.txt" . show) . Map.lookup (V2 10 10) . path

isInputSame :: IO ()
isInputSame = do
    a <- getActualCave testPath
    b <- input "inputs/test/15b.txt"
    print $ a == b

printReturn :: Show a => a -> IO a
printReturn a = print a $> a

problemOne :: IO ()
problemOne = input inputPath >>= (printReturn >=> writeFile "./inputs/test/parsedout/15a.txt" . show) . Map.lookup (V2 100 100) . path (V2 100 100)

problemTwo :: IO ()
problemTwo = getActualCave inputPath >>= (printReturn >=> writeFile "./inputs/test/parsedout/15b.txt" . show) . Map.lookup (V2 500 500) . path (V2 500 500)

endNode :: Point
endNode = V2 500 500

startNode :: Point
startNode = V2 1 1

{-
    Ok - Part 1
    time                 74.93 ms   (70.64 ms .. 80.82 ms)
                         0.992 R²   (0.985 R² .. 1.000 R²)
    mean                 72.00 ms   (71.17 ms .. 74.86 ms)
    std dev              2.434 ms   (660.9 μs .. 4.111 ms)


    Okish - Part 2
    time                 4.445 s    (3.574 s .. 5.082 s)
                         0.994 R²   (0.992 R² .. 1.000 R²)
    mean                 4.317 s    (3.922 s .. 4.471 s)
    std dev              283.4 ms   (19.74 ms .. 357.6 ms)
    variance introduced by outliers: 19% (moderately inflated)
-}
-- | Dijkstra, but I didn't know it had a name
path :: Point -> Cave -> Nodes
path endpt cave = go startNode Set.empty (Map.insert (V2 1 1) (Knot (Just 0) (V2 1 1)) $ Map.mapWithKey (const . Knot Nothing) cave) Heap.empty
    where
    go :: Point -> Set Point -> Nodes -> Heap.Heap Knot -> Nodes
    go _ _ nodes _ | isJust . _val $ nodes Map.! endpt       = nodes
    go currentCave seen nodes heap | currentCave `Set.member` seen =
                                    let p = _point . Heap.minimum $ heap
                                    in go p seen nodes . Heap.deleteMin $ heap
    go currentCave seen nodes heap = do
        let seen'   = Set.insert currentCave seen
            nbs     = mapMaybe (fmap toJust . (`Map.lookup` nodes)) . getNeighbors $ currentCave
            heap'   = Heap.union (Heap.fromList nbs) heap
            nodes'  = Map.union (Map.fromList $ fmap _point nbs `zip` nbs) nodes
            p       = _point $ Heap.minimum heap'
            in go p seen' nodes' (Heap.deleteMin heap')
        where
        Knot val _ = nodes Map.! currentCave
        toJust (Knot Nothing a) = Knot (fmap (+( cave Map.! a   )) val) a
        toJust k@(Knot (Just i) a)
            | i > (+pathDanger) (fromJust val) = Knot (fmap (+(cave Map.! a)) val) a
            | otherwise               = k
            where
            pathDanger = cave Map.! a

