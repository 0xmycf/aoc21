module DayEleven.DayEleven
( mainDayEleven
, testDayEleven
) where

import           Common.Lib  (getAllNeighbs, getLines, mapIdx)
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe  (isJust)
import           Linear      (V2 (V2))
import Prelude hiding (all)
import Data.List ((\\))


inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayEleven.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayEleven.txt"

type Coordinate = V2 Int

-- | creates a map of key (row, column) (keys are V2) to Int (the octopus)
input :: FilePath -> IO (Map Coordinate Int)
input path = let inputs = mapIdx (\a idx -> mapIdx (\b idx1 -> (V2 (idx + 1) (idx1 + 1), read [b])) a) <$> getLines path
            in do
            cl <- concat <$> inputs
            pure $ Map.fromList cl

mainDayEleven :: IO ()
mainDayEleven = putStrLn "Day Eleven..." >> problemOne >> problemTwo >> putStrLn "Day Eleven over.\n "

testDayEleven :: IO ()
testDayEleven = do
    putStrLn "Test Day Eleven..."
    inp <- input testPath
    let foo = step $ inp
    print $ step $ inp
    writeFile "./inputs.txt" (show foo)
    putStrLn "Test Day Eleven over.\n"


problemOne :: IO ()
problemOne = print "to be impl"

problemTwo :: IO ()
problemTwo = print "to be impl"


step :: Map Coordinate Int -> Map Coordinate Int
-- error his here, because folded map gets not changed obviously!
step mp = fst .  Map.foldlWithKey (\a@(acc, n) key val -> if val + 1 > 9 then flash a [key] else (Map.insert key (val + 1) acc, n)) (mp, []) $ mp
    where
    flash :: (Map Coordinate Int, [Coordinate]) -> [Coordinate] -> (Map Coordinate Int, [Coordinate])
    flash all []          = all
    flash (mp,acc) [c] =
        let neighbs = (filter justFilter . getAllNeighbs $ c)
            in go (Map.insert c 0 mp) neighbs (c : acc)
    flash _ _ = error "never happens"
    go :: Map Coordinate Int -> [Coordinate] -> [Coordinate] -> (Map Coordinate Int, [Coordinate])
    go m [] set = (m,set)
    go map' nbs@(x:xs) set
        | map' Map.! x + 1 <= 9 = go (Map.insertWith (+) x 1 map') xs set
        | map' Map.! x + 1 > 9 && x `notElem` set = flash (map', set) [x]
        --  | map' Map.! x + 1 > 9 && x `notElem` set = go (Map.insert x 0 map') ((filter justFilter . getAllNeighbs $ x) ++ xs) (x : set)
        | x `elem` set = go map' xs set
        | otherwise = error $ "map " ++  show map' ++ " nbs " ++ show nbs ++ " set " ++ show set
    justFilter x = let x' = x `Map.lookup` mp in isJust x'


        {-
            What needs to happen inside flash: 
            Values greater then nine need to be set to 0 unless they have been set to 0 previously, in which case they are inside acc
            Every other value will be increased by one, unless they will go beyond 9, then the first rule applies
            If any value hits a number greater than 9, it will also flash
        -}
