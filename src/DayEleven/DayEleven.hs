module DayEleven.DayEleven
( mainDayEleven
, testDayEleven
) where

import           Common.Lib (frequencyMap, getAllNeighbs, getLines, mapIdx)
import           Data.List  (unfoldr)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (isJust)
import qualified Data.Set   as Set
import           Linear     (V2 (V2))
import           Prelude    hiding (all)


inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayEleven.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayEleven.txt"

testPath2 :: FilePath
testPath2  = "./inputs/test/DayEleven2.txt"

type Coordinate = V2 Int
type Octopi     = Map Coordinate Int

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
    inp2<- input testPath2
    let foo = fmap setToZero . flash . step $ inp
        foo2= inp2
    print foo
    print $ fmap (==foo2) foo
    print $ sum . take 100 . unfoldr (Just . fullStep) $ inp
    print $ length . takeWhile (/=100) . unfoldr (Just . fullStep) $ inp
    --writeFile "./inputs.txt" (show foo)
    putStrLn "Test Day Eleven over.\n"

problemOne :: IO ()
problemOne = do
    inp <- input inputPath
    print $ sum . take 100 . unfoldr (Just . fullStep) $ inp

problemTwo :: IO ()
problemTwo = do
    inp <- input inputPath
    print $ 1 + (length . takeWhile (/=100) . unfoldr (Just . fullStep) $ inp )

-- | simulates a full step
fullStep :: Octopi -> (Int, Octopi)
fullStep = fmap setToZero . flash . step

-- | adds one level to each octopus
step :: Octopi -> Octopi
step = fmap (+1)

-- | takes in the stepped octopi board, then flashes the flashy ones
-- | and returns a tuple of the amount of flashes and the updated board
flash :: Octopi -> (Int, Octopi)
flash octo = (amnt, octo'')
    where
    (flashy, sleepy) = Map.partition (>9) octo
    neighbs = frequencyMap . concatMap (filter (justFilter octo) . getAllNeighbs) . Map.keys $ flashy
    amnt    = length flashy + amnt2
    octo'   = Map.unionWith (+) neighbs octo
    (amnt2 , octo'')  = moreFlashes octo' sleepy

moreFlashes :: Octopi -> Octopi -> (Int, Octopi)
moreFlashes = go 0
    where
    go :: Int -> Octopi -> Octopi -> (Int, Octopi)
    go i octo sl
        | null flashy2 = (i, octo)
        | otherwise     = go (i + length flashy2) octo' sl'
        where
        (flashy2, sl')  = Map.partition (>9) $ Map.restrictKeys octo (Set.fromList (Map.keys sl))
        neighbs2 = frequencyMap . concatMap (filter (justFilter octo) . getAllNeighbs) . Map.keys $ flashy2
        octo'    = Map.unionWith (+) octo neighbs2


-- | The function that sets all values greater than 9 to zero
-- | NOTE: Should be applied after all flashes!
setToZero :: Octopi -> Octopi
setToZero octo = (0 <$ tooHigh) `Map.union` octo
    where
    (tooHigh, _) = Map.partition (>9) octo

-- | filters through allNeighbors to the neighbors which are actually in the map
justFilter :: Ord k => Map k a -> k -> Bool
justFilter mp x = let x' = x `Map.lookup` mp in isJust x'
