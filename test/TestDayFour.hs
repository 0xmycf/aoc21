module TestDayFour
( testDayFourSuit
) where

import Lib (getLines)
import qualified DayFourSrc as DFS
    ( prepare,
      mkBoard,
      inputBoards,
      parsedBoards,
      pToList,
      parseNums,
      BoardNumber (DFS.Bn, marked, number), bToP, drawAndMark, Board, Game (Win, Queue), isQueue
      )

import qualified Data.Map as Map
import qualified Data.Matrix as M
import Data.Map (Map)
import DayFourSrc (check, BoardNumber (marked, number), concentrate, bToP, parsedBoards, inputBoards, parseNums, pToList, prepare, mkBoard)
import Data.Map.Lazy ((!))
import GHC.Base (IO(IO))

inputPathSamples :: String
inputPathSamples = "test/samples/input3.txt"

samplelistAsString :: [Char]
samplelistAsString = "74951117232014212410161361525122218208193261"

testDayFourSuit :: IO ()
testDayFourSuit = do
    lines <- getLines inputPathSamples
    mapM_ print (inputBoards lines) -- (\l -> ',' `elem` l || l == ""
    print $ mkBoard . prepare .  map words . take 5 $ inputBoards lines
    print $ parsedBoards (inputBoards lines)
    print $ map mkBoard . parsedBoards $ inputBoards lines
    let mapped = bToP $ parsedBoards $ inputBoards lines
    print $ mapped
    case Map.lookup 16777215 mapped of
     Nothing -> error "Nothing while getting the first input of the Map!"
     Just ma -> print $ M.unsafeGet 1 1 ma
    print $ "Parsing input list..."
    print $ pToList (parseNums lines)
    print $ pToList (parseNums lines) == [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
    -- print $ "Draw and Mark Tests"
    -- print $ DFS.drawAndMark mapped (pToList (parseNums lines))
    print $ "TEST Draw and Mark Tests TEST"
    g <- drawAndMark mapped (pToList (parseNums lines))
    case g of
      DFS.Win      n -> print $ show (n == 4512) ++  " " ++ show n
      DFS.Queue map  -> print $ map
    print $ "Semi Group tests"
    print $ "t t t - 0"
    -- print $ (DFS.Bn 1 True <> DFS.Bn 2 True) <> DFS.Bn 3 True
    -- print $ DFS.Bn 1 True <> (DFS.Bn 2 True <> DFS.Bn 3 True)
    print $ (DFS.Bn 1 True <> DFS.Bn 2 True) <> DFS.Bn 3 True == DFS.Bn 1 True <> (DFS.Bn 2 True <> DFS.Bn 3 True)
    print $ "f f f - 6"
    -- print $ (DFS.Bn 1 False <> DFS.Bn 2 False) <> DFS.Bn 3 False
    -- print $ DFS.Bn 1 False <> (DFS.Bn 2 False <> DFS.Bn 3 False)
    print $ (DFS.Bn 1 False <> DFS.Bn 2 False) <> DFS.Bn 3 False == DFS.Bn 1 False <> (DFS.Bn 2 False <> DFS.Bn 3 False)
    print $ "f t t - 1"
    -- print $ (DFS.Bn 1 False <> DFS.Bn 2 True) <> DFS.Bn 3 True
    -- print $ DFS.Bn 1 False <> (DFS.Bn 2 True <> DFS.Bn 3 True)
    print $ (DFS.Bn 1 False <> DFS.Bn 2 True) <> DFS.Bn 3 True == DFS.Bn 1 False <> (DFS.Bn 2 True <> DFS.Bn 3 True)
    print $ "t f t - 2"
    -- print $ (DFS.Bn 1 True <> DFS.Bn 2 False) <> DFS.Bn 3 True 
    -- print $ DFS.Bn 1 True <> (DFS.Bn 2 False <> DFS.Bn 3 True)
    print $ (DFS.Bn 1 True <> DFS.Bn 2 False) <> DFS.Bn 3 True == DFS.Bn 1 True <> (DFS.Bn 2 False <> DFS.Bn 3 True)
    print $ "t t f - 3"
    -- print $ (DFS.Bn 1 True <> DFS.Bn 2 True) <> DFS.Bn 3 False  
    -- print $ DFS.Bn 1 True <> (DFS.Bn 2 True <> DFS.Bn 3 False)
    print $ (DFS.Bn 1 True <> DFS.Bn 2 True) <> DFS.Bn 3 False == DFS.Bn 1 True <> (DFS.Bn 2 True <> DFS.Bn 3 False)
    print $ "f f t - 3"
    -- print $ (DFS.Bn 1 False <> DFS.Bn 2 False) <> DFS.Bn 3 True  
    -- print $ DFS.Bn 1 False <> (DFS.Bn 2 False <> DFS.Bn 3 True)
    print $ (DFS.Bn 1 False <> DFS.Bn 2 False) <> DFS.Bn 3 True == DFS.Bn 1 False <> (DFS.Bn 2 False <> DFS.Bn 3 True)
    print $ "t f f - 5"
    -- print $ (DFS.Bn 1 True <> DFS.Bn 2 False) <> DFS.Bn 3 False  
    -- print $ DFS.Bn 1 True <> (DFS.Bn 2 False <> DFS.Bn 3 False)
    print $ (DFS.Bn 1 True <> DFS.Bn 2 False) <> DFS.Bn 3 False == DFS.Bn 1 True <> (DFS.Bn 2 False <> DFS.Bn 3 False)
    print $ "f t f - 4"
    -- print $ (DFS.Bn 1 False <> DFS.Bn 2 True) <> DFS.Bn 3 False  
    -- print $ DFS.Bn 1 False <> (DFS.Bn 2 True <> DFS.Bn 3 False)
    print $ (DFS.Bn 1 False <> DFS.Bn 2 True) <> DFS.Bn 3 False == DFS.Bn 1 False <> (DFS.Bn 2 True <> DFS.Bn 3 False)
    print $ "Ord test"
    print $ DFS.Bn 1 False < DFS.Bn 2 True
    print $ DFS.Bn 1 False > DFS.Bn 2 True
    print $ DFS.Bn 1 False == DFS.Bn 2 True
    print $ "row check"
    let b = M.fromList 3 3 (replicate 9 (DFS.Bn 1 False))
    let b = M.fromList 3 3 [DFS.Bn 1 True, DFS.Bn 1 True, DFS.Bn 1 True, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False, DFS.Bn 1 False]
    print $ b
    let rowCheck = any (\v ->  and . M.toList . fmap marked . M.submatrix v v 1 3 $ b) [1..3]
    let colCheck = any (\v ->  and . M.toList . fmap marked . M.submatrix 1 3 v v $ b) [1..3]
    print $ rowCheck
    print $ colCheck
    print $ number (foldl (<>) (DFS.Bn 0 True) . M.toList $ b)


sample :: [[[Char]]]
sample = [
    ["22","13","17","11","0"],
    ["8","2","23","4","24"],
    ["21","9","14","16","7"],
    ["6","10","3","18","5"],
    ["1","12","20","15","19"]
    ]


prepared :: [Int]
prepared = prepare sample

-- | Tests drawAndMark

-- | Takes in the Boards as well as the RNG Num List
drawAndMark :: Map Integer (DFS.Board BoardNumber) -> [Int] -> IO DFS.Game -- Always returns Queue.. Which shouldn't happen...
drawAndMark ms = mark (DFS.Queue ms)
  where
    mark :: DFS.Game -> [Int] -> IO DFS.Game
    mark g []                 = if DFS.isQueue g then error "No winner!" else return g
    mark (DFS.Win a) x        = return $ DFS.Win a
    mark (DFS.Queue m) (x:xs) = do
        print $ "m"
        print $ m
        print $ "x"
        print $ x
        print $ "xs"
        print $ xs
        mark (Map.foldrWithKey (\ k _ g -> do
            case g of
                (DFS.Win a)     -> DFS.Win a
                (DFS.Queue map) -> if check x k then notify x k map else g)
            (DFS.Queue m) m) xs


notify :: Int -> Integer -> Map Integer (DFS.Board BoardNumber) -> DFS.Game
notify i k ms = bMark (ms ! k)
  where
    bMark :: DFS.Board BoardNumber -> DFS.Game
    bMark b = do
      -- This part should get thoroughly tested
       let new = M.mapPos swapAndNotify b
       let rowCheck = any (\v ->  and . M.toList . fmap marked . M.submatrix v v 1 5 $ new) [1..5]
       let colCheck = any (\v ->  and . M.toList . fmap marked . M.submatrix 1 5 v v $ new) [1..5]
       if rowCheck || colCheck
       then DFS.Win (i * concentrate new)
       else DFS.Queue (Map.adjust (const new) k ms)
        where
          swapAndNotify _ a =
            if number a == i
            then DFS.Bn (number a) True
            else a