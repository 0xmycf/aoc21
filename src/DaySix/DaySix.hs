{-# OPTIONS_GHC -Wno-type-defaults #-}
module DaySix.DaySix
( mainDaySix
, testDaySix
, problemOne
, problemTwo
) where

import Common.Lib (parse)
import Linear (V2(..))
import Data.List (group, sort)

import qualified Data.Map    as Map
import qualified Text.Parsec as Parsec
import Data.Map (Map)

inputPath :: String
inputPath = "./inputs/DaySix.txt"

testPath :: String
testPath  = "./inputs/test/DaySix.txt"

mainDaySix :: IO ()
mainDaySix = putStrLn "Day Six..." >> problemOne >> problemTwo >> putStrLn "Day Six over.\n "

testDaySix :: IO ()
testDaySix = do
    raw   <- inputParser testPath
    case raw of
      Left  pe -> print pe
      Right ns -> let inter = interpretInput ns in do
          print $ map (fmap (\x -> V2 (length x) 0)) . zipWith (\_ a -> (head a, a)) [0..] . group . sort $ ns
          print   inter
          print $ fishies inter
          print $ fishies (fishies inter)
          print $ (sum . Map.elems . foldr (\_ p -> fishies p) inter) [1..80]

problemOne :: IO ()
problemOne = do
    raw   <- inputParser inputPath
    case raw of
      Left  pe -> print pe
      Right ns -> let inter = interpretInput ns in do
          print $ (sum . Map.elems . foldr (\_ p -> fishies p) inter) [1..80]

problemTwo :: IO ()
problemTwo = do
    raw   <- inputParser inputPath
    case raw of
      Left  pe -> print pe
      Right ns -> let inter = interpretInput ns in do
          print $ (sum . Map.elems . foldr (\_ p -> fishies p) inter) [1..256]

inputParser :: String -> IO (Either Parsec.ParseError [Int])
inputParser s = fmap (fmap read) . parse (Parsec.sepBy (Parsec.many Parsec.digit) (Parsec.char ',')) <$> readFile s

-- | makes sense doesn't it?
interpretInput :: [Int] -> Map Int Fish
interpretInput inp = let mp  = Map.fromList ([0..8] `zip` repeat (V2 0 0))
                         srt = map (fmap (\x -> V2 (length x) 0)) . zipWith (\_ a -> (head a, a)) [0..] . group . sort $ inp in
                              Map.mapWithKey (\k _ -> foldl (\c (a',b') -> if k == a' then b' else c) (V2 0 0) srt) mp

{-
    Before I started I researched some papers about models for individual based exponential population growth.
    I found this paper, so my solution is based on it: https://arxiv.org/pdf/2005.03974.pdf (by Luis R. T. Neves, Leonardo Paulo Maia)

    The basic idea is to have a List of size n, where n is the max value of replication cycles.
    In our case n = max {6, 8} = 8.

    Because I think that part 2 will involve them dying or some other sort of limitation I will also account for that.

    Let STATES-t = (X0-t, ... , X8-t), where Xj-t is the amount of fish with replication age of j (j in 0..8) (t in N)
    The replication age is the natural number (0..8) of days which have to pass before the fish replicates.
        Eg. 2 would mean the fish would replicate in 2 days (it will firstly lower to 1 and then lower to 0 and become 6 again)

    To adjust the model for the event of death, we will simply transform each Xj to a two (or more) dimensional vector.
    (f, s), where f will represent the replication age and s will represent the dying age.

    We will now define the function which computes the new values for t -> t+1
        f(Xj-(t+1)) = X(j+1)-t      , if j!=8 and j!=6
        f(Xj-(t+1)) = X1-t + X7-t   , if j=6
        f(Xj-(t+1)) = X1-t          , if j=8


    The hardest part about this day was actually writing the interpretInput function, it took me some mental acrobatics and ghci hacks to figure that one out.
    The other part which took me quite long was to find a good data structure. A Map turned out to be the best.
-}

type Fish = V2 Int

fishies :: Map Int Fish -> Map Int Fish
fishies mp = Map.mapWithKey f mp
    where
        f k _   | k/=8 && k/=6    = mp Map.! (k+1)
                | k==6            = mp Map.! 0 + (mp Map.! 7)
                | k==8            = mp Map.! 0
                | otherwise       = error "Something went Wrong"


{-
    Bench problemOne
    time                 507.3 μs   (493.1 μs .. 523.1 μs)
                         0.991 R²   (0.985 R² .. 0.995 R²)
    mean                 519.5 μs   (505.5 μs .. 543.0 μs)
    std dev              57.55 μs   (38.65 μs .. 81.94 μs)
    variance introduced by outliers: 80% (severely inflated)

    Bench problemTwo
    time                 702.2 μs   (693.6 μs .. 710.9 μs)
                         0.998 R²   (0.997 R² .. 0.999 R²)
    mean                 699.8 μs   (692.9 μs .. 709.2 μs)
    std dev              27.84 μs   (21.38 μs .. 37.86 μs)
    variance introduced by outliers: 32% (moderately inflated)
-}