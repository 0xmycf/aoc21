{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module DayFourteen.DayFourteen
( mainDayFourteen
, testDayFourteen
) where

import           Common.Lib    (frequencyMap, getLines)
import           Data.List     (unfoldr, sort)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import System.Directory.Internal.Prelude (exitFailure)

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

problemOne :: IO ()
problemOne = do
    (str, tmp) <- input testPath
    print . factory str tmp $ 10

-- don't do do that
problemTwo :: IO ()
problemTwo = do
    (str, tmp) <- input inputPath
    getline <- print "Are you sure you want to execute Day 14 Part 2?" *> getLine 
    case getline of
        "True"  -> do
            print . factory str tmp $ 1
            print . factory str tmp $ 2
            print . factory str tmp $ 3
            print . factory str tmp $ 4
            print . factory str tmp $ 5
            print . factory str tmp $ 6
            print . factory str tmp $ 7
            print . factory str tmp $ 8
            print . factory str tmp $ 9
            print . factory str tmp $ 10
            print . factory str tmp $ 11
            print . factory str tmp $ 12
            print . factory str tmp $ 13
            print . factory str tmp $ 14
            print . factory str tmp $ 15 -- until here its okay, after that it gets insanely slow
            print . factory str tmp $ 20
            print . factory str tmp $ 40
        _       -> exitFailure

factory :: Polymer -> Map PC Char -> Int -> Int
factory str tmp i = (\v -> maximum v - minimum v )
            . Map.elems . frequencyMap . foldl toString "" . Map.toList . last . take i . unfoldr (Just . printing tmp) $ str
        where -- . fmap halveIt 
        printing tmp v = let a = printPolymer v tmp in (a,a)
        toString acc (PC (a:b:_), i) = let ih = (i `div` 2) + 1 in 
            acc ++ replicate ih a ++ replicate ih b
        halveIt = \x -> if odd x then (x `div` 2) + 1 else x `div` 2

-- | TODO find a better data structure to represent this and speed it up
printPolymer :: Polymer -> Template -> Polymer
printPolymer poly tmp = poly'
    where
    -- faster but not fast enough...
    -- need to get rid of the list once and for all, maybe use vectors here? directly using a Map would be nicer though
    newComps = foldl (\acc v -> let c = tmp Map.! v; (a:b:_) = _comp v in putInAcc (PC [a,c]) (PC [c,b]) acc v) [] $ Map.keys poly
    -- newComps = concat . concatMap (\v -> let c = tmp Map.! v; (a:b:_) = _comp v in replicate (amnt v) [PC [a,c], PC [c,b]]) $ Map.keys poly
    poly'    = frequencyMap newComps
    amnt v   = poly Map.! v
    putInAcc pc1 pc2 acc v = foldl (\b _ -> pc1 : pc2 :  b) acc [1..(amnt v)] --acc 

printPolymer' poly tmp = sort newComps
    where
    newComps = concat . concatMap (\v -> let c = tmp Map.! v; (a:b:_) = _comp v in replicate (amnt v) [PC [a,c], PC [c,b]]) $ Map.keys poly
    poly'    = frequencyMap newComps
    amnt v   = poly Map.! v

charsToPC :: Char -> Char -> PC
charsToPC c c1 = PC [c,c1]
