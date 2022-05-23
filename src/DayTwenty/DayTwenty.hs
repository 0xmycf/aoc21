{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DayTwenty.DayTwenty
( mainDayTwenty
, testDayTwenty
, problemTwo
) where

import           Common.Lib               (Point, mapIdx)
import           Control.Concurrent.Async (mapConcurrently)
import           Data.List                (sort, unfoldr)
import           Data.List.Split          (splitOn)
import qualified Data.Map.Lazy            as Map
import           Debug.Trace              (trace)
import           GHC.List                 (foldl')
import           Linear                   (V2 (V2))

strace :: Show a => (b -> a) -> b -> b
strace f a = trace (show $ f a) a

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayTwenty.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayTwenty.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DayTwenty.txt"

type Input = (Alg, Img)
type Alg   = Map.Map Int Bit
type Gimg  = Map.Map Point Bit

data Img   = Img
  { gimg  :: Gimg
  , ginfs :: Bit
  }

mapGimg :: (Gimg -> Gimg) -> Img -> Img
mapGimg f (Img img infs) = Img (f img) infs

opposite :: Bit -> Bit
opposite Zero = One
opposite One  = Zero

data Bit = Zero | One deriving (Eq, Show, Enum)

-- | parses the input depending on the FilePath given
input :: FilePath -> IO Input
input path = let inputs = parse <$> readFile path
             in  inputs

mainDayTwenty :: IO ()
mainDayTwenty = putStrLn "Day Twenty..." >> input inputPath >>= print . problemOne >> putStrLn "Day Twenty over.\n "

-- concurrent haskell :handsup:
problemTwo :: IO ()
problemTwo = input inputPath >>= (\(alg, img) ->  sequenceA . unfoldr (\((b, c) :: (IO Img, Int)) -> if c == 51 then Nothing else Just (b, (fmapimg alg b , c + 1))) $ (pure img, 0))
              >>= (print . countLit . (!! 50))
  where fmapimg alg b = b >>= (`mapImgConcurr` alg)


testDayTwenty :: IO ()
testDayTwenty = putStrLn "Day Twenty..." >> input testPath >>= print . problemOne >> putStrLn "Day Twenty over.\n "

problemOne :: Input -> String
problemOne (alg, img) = show . countLit $ flip mapImg alg $ mapImg img alg

mapImg :: Img -> Alg -> Img
mapImg wimg@(Img img infs) alg = Img (Map.mapWithKey (\key _ -> (alg Map.!) . bitArrToInt . retrieveNine wimg $ key) expanded) (opposite infs)
  where expanded = Map.union img boardermap
        boardermap = Map.fromList . map (,infs) $ [V2 a b | a <- boarder, b <- boarder]
        mapToIdx = map ((\(V2 _ (x :: Int)) -> x ) . fst) . Map.toList $ img
        maxIndex = maximum mapToIdx
        minIndex = minimum mapToIdx
        boarder  = [minIndex-1..maxIndex+1]

mapImgConcurr :: Img -> Alg -> IO Img
mapImgConcurr wimg@(Img img infs) alg = do
  mapped <- mapConcurrently go (Map.toList expanded)
  pure . flip Img (opposite infs) $! Map.fromList mapped
    where go       = pure . (\(key, _) -> (key, (alg Map.!) . bitArrToInt . retrieveNine wimg $ key))
          expanded = Map.union img boardermap
          boardermap = Map.fromList . map (,infs) $ [V2 a b | a <- boarder, b <- boarder]
          mapToIdx = map ((\(V2 _ (x :: Int)) -> x ) . fst) . Map.toList $ img
          maxIndex = maximum mapToIdx
          minIndex = minimum mapToIdx
          boarder  = [minIndex-1..maxIndex+1]

retrieveNine :: Img -> Point -> [Bit]
retrieveNine (Img img infs) (V2 row col) = fmap (go . (`Map.lookup` img)) . sort $ [key | a <- directions, b <- directions, let key = V2 (row + a) (col + b)]
  where directions = [1, 0 , -1]
        go Nothing  = infs
        go (Just x) = x

bitArrToInt :: [Bit] -> Int
bitArrToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0

countLit :: Img -> Int
countLit  = length . filter (==One) . fmap snd . Map.toList . gimg

{-
  Parsing
-}

-- first alg, then img
parse :: String -> Input
parse str = (Map.fromList . mapIdx (\val idx -> (idx, toBit val)) $ first, Img img Zero)
  where [first, second] = splitOn "\n\n" str
        toBit '.' = Zero
        toBit '#' = One
        toBit c   = error $ "Not . or # char (" ++ [c] ++ ")"
        img =  Map.fromList . concat . mapIdx (\val idx1 -> mapIdx (\val2 idx2 -> (V2 idx1 idx2, toBit val2)) val) . lines $ second

-- saves an Img to a file for debugging
antiParse :: Img -> IO ()
antiParse (Img img _) = writeFile outPath $ unlines . groupX (length [minIndex..maxIndex]) . map (\val -> if snd val == Zero then '.' else '#') . Map.toList $ img
  where mapToIdx = map ((\(V2 _ (x :: Int)) -> x ) . fst) . Map.toList $ img
        maxIndex = maximum mapToIdx
        minIndex = minimum mapToIdx
        groupX :: Int -> [a] -> [[a]]
        groupX x xs = reverse $ go xs [] []
          where go [] acc ans = reverse acc : ans
                go (y:ys) acc ans = if length acc - x == 0 then go ys [y] (reverse acc : ans) else go ys (y:acc) ans

-- debug and stuff that saves the maps
solution :: (Alg, Img) -> Img
solution (alg, img) = flip mapImg alg $ mapImg img alg

solDayTwenty, solDayTwenty2 :: IO ()
solDayTwenty = putStrLn "Day Twenty..." >> input inputPath >>= antiParse . solution >> putStrLn "Day Twenty over.\n "
solDayTwenty2 = putStrLn "Day Twenty..." >> input inputPath >>= print . countLit . solution >> putStrLn "Day Twenty over.\n "

saveSomething :: IO ()
saveSomething = (\(alg,img) -> antiParse . (!! 2) . unfoldr (\(b :: Img) -> Just (b, fmapimg alg b)) $ img) =<< input inputPath
  where fmapimg alg = flip mapImg alg

giveSomething :: IO ()
giveSomething = (\(alg,img) -> print . countLit . (!! 2) . unfoldr (\(b :: Img) -> Just (b, fmapimg alg b)) $ img) =<< input inputPath
  where fmapimg alg = flip mapImg alg

saveVanilla :: IO ()
saveVanilla = input inputPath >>= antiParse . snd

acutalSolution :: IO ()
acutalSolution = putStrLn "Day Twenty..." >> readFile outPath >>= print . superParse >> putStrLn "Day Twenty over.\n "
  where superParse = length . filter (=='#')
