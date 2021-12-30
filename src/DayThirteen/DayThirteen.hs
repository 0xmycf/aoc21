{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DayThirteen.DayThirteen
( mainDayThirteen
, testDayThirteen
) where

import           Common.Lib      (getLines)
import           Data.Char       (toUpper)
import           Data.List       (nub, partition)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Linear          (V2 (V2))

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayThirteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayThirteen.txt"

-- | parses the input depending on the FilePath given
input :: FilePath -> IO (Paper, [Fold])
input path = let input = partition (\v -> head v == 'f') . filter (/=[]) <$> getLines path
             in do
                (folds, nums) <- input
                pure (toMap nums, toFold folds)

type Point = V2 Int
type Paper = Map Point Bool

data Axis = X | Y deriving (Read, Show, Eq)
data Fold = Fold {
    _axis :: Axis,
    _val  :: Int
} deriving (Show, Eq)

mainDayThirteen :: IO ()
mainDayThirteen = putStrLn "Day Thirteen..." >> problemOne >> problemTwo >> putStrLn "Day Thirteen over.\n "

testDayThirteen :: IO ()
testDayThirteen = do
    putStrLn "Test Day Thirteen..."
    inp <- input testPath
    print inp
    putStrLn "Test Day Thirteen over.\n"


problemOne :: IO ()
problemOne = do
    (paper, f:_) <- input inputPath
    let (yesFold, noFold) = Map.partitionWithKey (part f) paper
        yesFold'          = fmap fst . Map.toList $ yesFold -- using a map here was not the right choice haha
        noFold'           = fmap fst . Map.toList $ noFold
    print $ length $ nub $ noFold' ++ fmap (mirror f) yesFold'
    where
    part :: Fold -> Point -> Bool -> Bool
    part f (V2 x y) _ = case _axis f of
        X -> x > _val f
        Y -> y > _val f


problemTwo :: IO ()
problemTwo = print "to be impl"

{-
    Parsing
    Through string manipulation

    Works only on valid input for this problem.
-}

toMap :: [String] -> Paper
toMap xs = Map.fromList (xs' `zip` repeat True)
    where
    xs' = fmap ((\(x:y:_) -> V2 (read x) (read y)) . splitOn ",") xs

toFold :: [String] -> [Fold]
toFold = fmap folding
    where
    folding :: String -> Fold
    folding ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':a:'=':xs) = Fold {_axis = read [toUpper a] , _val = read xs}
    folding i = error $ "Invalid input: " ++ show i

{-
    Some basic geometry:
    A-|–A' , where g is a line. The interception of the perpendicular of g through A is called F.
      g                         A' is the mirrored point of A if the following is true:
                                        1. A = F = A' or A - F - A' (they're collinear)
                                        2. d(A,F) = d(F,A'), where d(P,P') is the distance from a Point P to a Point P'

    Finding a distance function:
    The euclidean distance (in R^2) would look like this:
    -- | euclidean distance
    -- dist :: Floating a => V2 a -> V2 a -> a
    -- dist (V2 a b) (V2 x y) = sqrt $ (a - x)^(2 :: Integer) + (b - y)^(2 :: Integer)

    This is not suited for our needs though, since our Points are Integer points
    and the distance will always be of type Integer, since they also lie on one straight line.

    We can simply use this function:
    abs $ (a - x) + (b - y)
    Why? Because either a = x or b = y will be true
    and the distance is just the absolute value of difference of the other components.
-}

-- | distance for integer grid points on one line
dist :: Num a => V2 a -> V2 a -> a
dist (V2 a b) (V2 x y) = abs $ (a - x) + (b - y)

-- | mirrors a Point p on the line g
--   this function is constrained to the problem (g lies either on the X or Y axis)
-- mirror :: Fold -> Point -> Point
mirror :: Fold -> Point -> Point
mirror (Fold Y v) p@(V2 x _) = V2 x (v - dist p (V2 x v))
mirror (Fold X v) p@(V2 _ y) = V2 (v - dist p (V2 v y)) y
