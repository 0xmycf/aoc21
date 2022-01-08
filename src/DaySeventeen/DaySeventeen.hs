{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module DaySeventeen.DaySeventeen
( mainDaySeventeen
, testDaySeventeen
) where

import           Common.Lib      (Point)
import           Control.Arrow   ((&&&))
import           Data.Foldable   (foldl')
import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Linear.V2       (V2 (..))

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DaySeventeen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DaySeventeen.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DaySeventeen.txt"

data Input = Input
            { x :: [Int]
            , y :: [Int]
            } deriving (Show)
            -- raw input looks like this 'target area: x=241..273, y=-97..-63'
type Velocity = Point

data SampleState = SState
    { velocity :: Velocity
    , point    :: Point
    } deriving (Show)


-- | parses the input depending on the FilePath given
--   I will write an actual Parser some other time...
--   let inputs = <$> readFile path
input :: FilePath -> IO Input
input path = pure $ Input { x=[241, 273], y=[-97, 63] }

mkField :: Input -> Set Point
mkField (Input [x0,x1] [y0,y1]) = S.fromList [V2 x' y' | x' <- [x0..x1], y' <- [y0..y1]]
mkField _ = error "Invalid Input!"

-- | This can be used to parse the range in the future
rangeParser :: String -> Set Int
rangeParser = S.fromList . toRange . splitOn ".."
    where
    toRange :: [String] -> [Int]
    toRange [a, b] = [(read a)..(read b)]
    toRange _      = error "Something went wrong at toRange"

oneStep :: SampleState -> SampleState
oneStep (SState (V2 0 y) (V2 a b)) = SState (V2 0 (y-1)) (V2 a (b+y))
oneStep (SState (V2 x y) (V2 a b)) = SState (V2 ((abs x - 1) * signum x) (y - 1)) (V2 (a+x) (b+y))

-- | This gives you the n-th velocity of the starting velocity
getIthVelo :: Int -> Velocity -> Velocity
getIthVelo n (V2 x y) = V2 ((abs x - n) * signum x * sig' (abs x - n)) (y - n)
    where sig' x'
            | x' <= 0   = 0
            | otherwise = 1

-- | given a Point this function returns the the first y negative trajectory point
--  (if the initial y velocity was positive)
func :: Point -> SampleState
func (V2 a n) = (!! (n * 2 + 2)) . iterate oneStep $ SState (V2 a n) 0

mainDaySeventeen :: IO ()
mainDaySeventeen = putStrLn "Day Seventeen..." >> input inputPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day Seventeen over.\n "

testDaySeventeen :: IO ()
testDaySeventeen = putStrLn "Day Seventeen..." >> input testPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day Seventeen over.\n "

problemOne :: Input -> Int
problemOne inp =
    let bounds = [V2 a b | b <- [1..(abs . minimum . y $ inp)], a <- [(1::Int)..(maximum . x $ inp)]]
    in  triag . foldl' findMax 0 $ bounds
    where
    field  = mkField inp
    findMax :: Point -> Point -> Point
    findMax acc p
        | (point . func $ p) `S.member` field
        && triag p > triag acc                = p
        | otherwise                           = acc
    triag :: Point -> Int
    triag (V2 _ y') = y' * (y' + 1) `div` 2

problemTwo :: Input -> String
problemTwo = const "to be impl"
