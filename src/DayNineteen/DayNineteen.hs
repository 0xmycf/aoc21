module DayNineteen.DayNineteen
( mainDayNineteen
, testDayNineteen
) where

import           Common.Lib            (Point, frequencyMap, getLines, updiv)
import           Control.Arrow         ((&&&))
import           Control.Monad         (guard)
import           Data.Either           (partitionEithers)
import           Data.Foldable         (Foldable (toList))
import           Data.Functor          (($>), (<&>))
import           Data.Functor.Identity (Identity)
import           Data.List             (group, intersect, nub, sort, transpose,
                                        uncons, (\\))
import qualified Data.List             as List
import           Data.List.Split       (splitOn)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes, fromJust, isJust)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Linear.Matrix         as Mat (M33, (!*))
import           Linear.V3             (V3 (..))
import qualified Text.Parsec           as Parsec

type Parser u a = Parsec.ParsecT String u Identity a

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayNineteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayNineteen.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DayNineteen.txt"

type Input = [[V3 Int]]

{-
  After many many tries and failures at writing this solution i looked up
  this: https://gitlab.com/sakisan/adventofcode/-/blob/2021/Haskell/Day19.hs
  I oriented myself on it, at first only briefly, but after many many more failures
  more closely. Its now almost the same, but slightly different.
  This slight difference causes an error in my code,
  their code runs fine.

  After some iterations of the compareAllScanners function the
  code crashes, due to an empty composite scanner, yet i cannot figure out why this would happen.

-}

-- I am not proud... This is not the math way
-- perms v =  fmap (Mat.!* v) [
-- after cross examination, i found that this somehow leads to an error
perms :: [M33 Int]
perms = [
            -- first id, +
            V3 (V3 1 0 0)
               (V3 0 1 0)
               (V3 0 0 1),
            --
            V3 (V3 1   0   0)
               (V3 0   0   1)
               (V3 0  (-1) 0),
            --
            V3 (V3 1 0  0  )
               (V3 0 0 (-1))
               (V3 0 1  0  ),
            --
            V3 (V3 1  0   0  )
               (V3 0 (-1) 0  )
               (V3 0  0  (-1)),
            -- first id, -
            V3 (V3 (-1) 0    0)
               (V3 0    (-1) 0)
               (V3 0    0    1),
            --
            V3 (V3 (-1) 0 0   )
               (V3 0    1 0   )
               (V3 0    0 (-1)),
            --
            V3 (V3 (-1) 0    0)
               (V3 0    0    1)
               (V3 0    1    0),
            --
            V3 (V3 (-1) 0    1   )
               (V3 0    0    (-1))
               (V3 0    (-1) 0   ),
            -- second first, +
            V3 (V3 0 (-1) 0)
               (V3 1 0    0)
               (V3 0 0    1),
            --
            V3 (V3 0 1 0   )
               (V3 1 0 0   )
               (V3 0 0 (-1)),
            --
            V3 (V3 0 0    1)
               (V3 1 0    0)
               (V3 0 (-1) 0),
            --
            V3 (V3 0 0 (-1))
               (V3 1 0 0   )
               (V3 0 1 0   ),
            -- second first, -
            V3 (V3 0    1 0)
               (V3 (-1) 0 0)
               (V3 0    0 1),
            --
            V3 (V3 0    (-1) 0   )
               (V3 (-1) 0    0   )
               (V3 0    0    (-1)),
            --
            V3 (V3 0    0 (-1))
               (V3 (-1) 0 0   )
               (V3 0    1 0   ),

            V3 (V3 0    0    1)
               (V3 (-1) 0    0)
               (V3 0    (-1) 0),
            -- third first, +
            V3 (V3 0 0 (-1))
               (V3 0 1 0   )
               (V3 1 0 0   ),
            --
            V3 (V3 0 0    1)
               (V3 0 (-1) 0)
               (V3 1 0    0),
            --
            V3 (V3 0 (-1) 0   )
               (V3 0 0    (-1))
               (V3 1 0    0   ),
            --
            V3 (V3 0 1 0)
               (V3 0 0 1)
               (V3 1 0 0),
            -- third first, -
            V3 (V3 0    0 1)
               (V3 0    1 0)
               (V3 (-1) 0 0),
            --
            V3 (V3 0    0    (-1))
               (V3 0    (-1) 0   )
               (V3 (-1) 0    0   ),
            --
            V3 (V3 0    (-1) 0)
               (V3 0    0    1)
               (V3 (-1) 0    0),
            --
            V3 (V3 0    1 0   )
               (V3 0    0 (-1))
               (V3 (-1) 0 0   )
                                    ]

testMiniScanner :: [V3 Int]
testMiniScanner = [
  V3 (-1) (-1) 1,
  V3 (-2) (-2) 2,
  V3 (-3) (-3) 3,
  V3 (-2) (-3) 1,
  V3 5 6 (-4),
  V3 8 0 7
                  ]


testScanner0 :: [V3 Int]
testScanner0 = [
    V3 404 (-588) (-901),
    V3 528 (-643) 409,
    V3 (-838) 591 734,
    V3 390 (-675) (-793),
    V3 (-537) (-823) (-458),
    V3 (-485) (-357) 347,
    V3 (-345) (-311) 381,
    V3 (-661) (-816) (-575),
    V3 (-876) 649 763,
    V3 (-618) (-824) (-621),
    V3 553 345 (-567),
    V3 474 580 667,
    V3 (-447) (-329) 318,
    V3 (-584) 868 (-557),
    V3 544 (-627) (-890),
    V3 564 392 (-477),
    V3 455 729 728,
    V3 (-892) 524 684,
    V3 (-689) 845 (-530),
    V3 423 (-701) 434,
    V3 7 (-33)(-71),
    V3 630 319 (-379),
    V3 443 580 662,
    V3 (-789) 900 (-551),
    V3 459 (-707) 401
            ]

testScanner1 :: [V3 Int]
testScanner1 = [
    V3 686 422 578,
    V3 605 423 415,
    V3 515 917 (-361),
    V3 (-336) 658 858,
    V3 95 138 22,
    V3 (-476) 619 847,
    V3 (-340) (-569) (-846),
    V3 567 (-361) 727,
    V3 (-460) 603 (-452),
    V3 669 (-402) 600,
    V3 729 430 532,
    V3 (-500) (-761) 534,
    V3 (-322) 571 750,
    V3 (-466) (-666) (-811),
    V3 (-429) (-592) 574,
    V3 (-355) 545 (-477),
    V3 703 (-491) (-529),
    V3 (-328) (-685) 520,
    V3 413 935 (-424),
    V3 (-391) 539 (-444),
    V3 586 (-435) 557,
    V3 (-364) (-763) (-893),
    V3 807 (-499) (-711),
    V3 755 (-354) (-619),
    V3 553 889 (-390)
           ]


-- | parses the input depending on the FilePath given
input :: FilePath -> IO Input
input path = parse <$> readFile path

mainDayNineteen :: IO ()
mainDayNineteen = putStrLn "Day Nineteen..." >> input inputPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day Nineteen over.\n "

testDayNineteen :: IO ()
testDayNineteen = putStrLn "Day Nineteen..." >> input testPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day Nineteen over.\n "

-- < 1496
-- < 748 fuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuck
-- == 350 but not by my solving...
-- > 193
problemOne :: [[V3 Int]] -> String
problemOne (i:np) = show . resolve . compareAllScanners [(i, 0)] [i] $ np
  where
    resolve mp = length . nub . concat . Map.keys $ mp
problemOne _   = "No input given"

type Scanner = [V3 Int]

permsToVariants :: Scanner -> [Scanner]
permsToVariants = transpose . fmap (\v -> fmap (Mat.!* v) perms)

compareTwoScanners :: [V3 Int] -> [V3 Int] -> [(Scanner, V3 Int)]
compareTwoScanners scannerA scannerB = [(fmap (+ offset) variant, offset)
                                        | variant <- permsToVariants scannerB
                                        , offset <- overlap scannerA variant
                                       ]
                                         where
                                           overlap :: Scanner -> Scanner -> [V3 Int]
                                           overlap a b = map fst . filter (\(_, count) -> count >= 12) . Map.toList . frequencyMap $
                                             (-) <$> a <*> b




safehead :: [a] -> Maybe a
safehead (x:_) = pure x
safehead []    = Nothing

compareAllScanners :: [(Scanner, V3 Int)] -> [[V3 Int]] -> [Scanner] -> Map.Map Scanner (V3 Int)
compareAllScanners visited _ [] = Map.fromList visited
compareAllScanners visited (ref:refs) scanners =
                    compareAllScanners (left ++ visited) (map fst left ++ refs) right
                      where
                        (left, right) = partitionEithers
                          [maybe (Right scanner) Left . safehead $ compareTwoScanners ref scanner | scanner <- scanners]
compareAllScanners visited [] scanners = error $ "refs empty" ++ ( show .length $ scanners ) ++ ":scanners|visited:" ++ (show .length $ visited)

problemTwo :: Input -> String
problemTwo = const "to be impl"

{-
    Parsing
-}

parse :: String -> [[V3 Int]]
parse = map (map toVec . tail . lines) . splitOn "\n\n"
  where
    toVec s = let [x,y,z] = splitOn "," s in V3 (read x) (read y) (read z)

