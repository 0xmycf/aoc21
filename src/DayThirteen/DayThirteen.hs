module DayThirteen.DayThirteen
( mainDayThirteen
, testDayThirteen
) where

import           Common.Lib      (getLines)
import           Data.Char       (toUpper)
import           Data.List       (nub, partition, foldl')
import           Data.List.Split (splitOn)
import           Data.Matrix     (Matrix)
import           Linear          (V2 (V2))
import qualified Data.Map        as Map
import qualified Data.Matrix     as Matrix

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayThirteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayThirteen.txt"

-- | slightly changed input to make part 2 easier
input' :: FilePath -> IO ([Point], [Fold])
input' path = let input = partition (\v -> head v == 'f') . filter (/=[]) <$> getLines path
             in input >>= \(folds, nums) -> pure (fmap ((\(x:y:_) -> V2 (read x) (read y)) . splitOn ",") nums, toFold folds)

type Point = V2 Int

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
    (points, _) <- input' testPath
    print points
    writeFile "inputs/test/parsedout/13.txt" (show . draw $ points)
    putStrLn "Test Day Thirteen over.\n"


problemOne :: IO ()
problemOne = input' inputPath >>= \(points, f:_) -> let (yesFold, noFold) = partition (part f) points
                                                    in print . length . nub $ noFold ++ fmap (mirror f) yesFold

problemTwo :: IO ()
problemTwo = input' inputPath >>= \(points, fs) -> writeFile "inputs/test/parsedout/13.txt" (show . draw . foldAll points $ fs)
    where
    foldAll :: [Point] -> [Fold] -> [Point]
    foldAll = foldl' \acc v ->
        let (yesFold, noFold) = partition (part v) acc
        in noFold ++ fmap (mirror v) yesFold

    -- For memories without foldl':
    -- foldAll ps []     = nub ps
    -- foldAll ps (f:fs) =
    --     let (yesFold, noFold) = partition (part f) ps in
    --         foldAll (noFold ++ fmap (mirror f) yesFold) fs

part :: Fold -> Point -> Bool
part f (V2 x y) = case _axis f of
    X -> x > _val f
    Y -> y > _val f
{-
    Parsing
    Through string manipulation

    Works only on valid input for this problem.
-}

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

-- | Distance for integer grid points on one line
dist :: Num a => V2 a -> V2 a -> a
dist (V2 a b) (V2 x y) = abs $ (a - x) + (b - y)

-- | mirrors a Point p on the line g.
--   This function is constrained to the problem (g lies either on the X or Y axis)
mirror :: Fold -> Point -> Point
mirror (Fold Y v) p@(V2 x _) = V2 x (v - dist p (V2 x v))
mirror (Fold X v) p@(V2 _ y) = V2 (v - dist p (V2 v y)) y


-- | draws the grid
--   Abusing the Matrix package to draw my things :)
--   Btw this function cost me 2 hours because I didn't realize that I need a Matrix with 51 51 instead of 50 50...
draw :: [Point] -> Matrix Int
draw ps = let mp  = Map.fromList ([V2 x y | x <- [0..50], y <- [0..50]] `zip` repeat False)
              mp' = Map.union (Map.fromList (ps `zip` repeat True)) mp
          in
          Matrix.transpose $ Matrix.fromList 51 51 ((\v -> if v then 1 else 0) <$> Map.elems mp')
