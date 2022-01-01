{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module DayFourteen.DayFourteen
( mainDayFourteen
, testDayFourteen
, problemOne
, problemTwo
) where

import           Common.Lib (frequencyMap, getLines)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           GHC.Base   (liftM2)

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayFourteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayFourteen.txt"

type Template = Map PC Char
type Polymer  = Map PC Int

-- Polymer component
newtype PC = PC {
    _comp :: String
} deriving (Show, Ord, Eq, Read)

-- | parses the input depending on the FilePath given
input :: FilePath -> IO (Polymer, Template)
input path = let inputs = filter (/=[]) <$> getLines path
             in do
                inp <- inputs
                let code = concat . take 1 $ inp
                    poly = frequencyMap $ zipWith charsToPC code (drop 1 code)
                    rest = Map.fromList . fmap toMap . drop 1 $ inp
                pure (poly, rest)
                where
                    toMap :: String -> (PC, Char)
                    toMap (a:b:' ':'-':'>':' ':c:_) = (PC [a,b], c)
                    toMap x = error $ "Parse error at: " ++ x

mainDayFourteen :: IO ()
mainDayFourteen = putStrLn "Day Fourteen..." >> problemOne >> problemTwo >> putStrLn "Day Fourteen over.\n "

testDayFourteen :: IO ()
testDayFourteen = do
    putStrLn "Test Day Fourteen..."
    (str, tmp) <- input testPath
    print $ printPolymer str tmp
    putStrLn "Test Day Fourteen over.\n"

{-
    I did a 'ad-hoc' bench:
    time                 739.5 μs   (639.2 μs .. 856.1 μs)
                         0.855 R²   (0.759 R² .. 0.967 R²)
    mean                 712.7 μs   (661.0 μs .. 834.6 μs)
    std dev              247.8 μs   (108.5 μs .. 413.4 μs)
-}

problemOne :: IO ()
problemOne = input inputPath >>= \(str, tmp) -> print . factory str tmp $ 10

{-
    Bench with 1-20 and 40
    time                 11.18 ms   (9.913 ms .. 12.94 ms)
                         0.874 R²   (0.785 R² .. 0.955 R²)
    mean                 11.17 ms   (10.54 ms .. 12.27 ms)
    std dev              2.153 ms   (1.329 ms .. 3.331 ms)
    variance introduced by outliers: 81% (severely inflated)

    Bench with only 40
    time                 3.142 ms   (2.973 ms .. 3.376 ms)
                         0.965 R²   (0.927 R² .. 0.997 R²)
    mean                 3.113 ms   (3.048 ms .. 3.321 ms)
    std dev              325.5 μs   (165.5 μs .. 620.3 μs)
    variance introduced by outliers: 68% (severely inflated)

    Bench without printing and without opening
    time                 3.019 ms   (2.893 ms .. 3.148 ms)
                         0.994 R²   (0.990 R² .. 0.999 R²)
    mean                 2.992 ms   (2.964 ms .. 3.030 ms)
    std dev              108.5 μs   (85.55 μs .. 146.2 μs)
    variance introduced by outliers: 21% (moderately inflated)
-}

-- | for bench
corrInp :: IO (Polymer, Template)
corrInp = input inputPath

problemTwo :: IO ()
problemTwo = corrInp >>= \(str, tmp) -> print . factory str tmp $ 40

factory :: Polymer -> Map PC Char -> Int -> Int
factory str tmp i = liftM2 (-) maximum minimum -- or with points: (\v -> maximum v - minimum v )
            . Map.elems . toCharFreq . (!! i) . iterate (`printPolymer` tmp) $ str
        where
        toCharFreq :: Polymer -> Map Char Int
        toCharFreq poly = (\x -> if odd x then (x `div` 2) + 1 else x `div` 2) <$> Map.foldlWithKey (\acc (PC (a:b:_)) val ->
            Map.unionWith (+) acc (Map.fromListWith (+) [(a, val), (b, val)])) Map.empty poly

printPolymer :: Polymer -> Template -> Polymer
printPolymer poly tmp = newComps
    where
    newComps = foldl (\acc v -> let c = tmp Map.! v; (a:b:_) = _comp v
        in Map.unionWith (+) acc (Map.fromList [(PC [a,c], amnt v), (PC [c,b], amnt v)])) Map.empty $ Map.keys poly
    amnt v   = poly Map.! v

charsToPC :: Char -> Char -> PC
charsToPC c c1 = PC [c,c1]
