module TestDayFour
( testDayFourSuit
) where

import Lib (getLines)
import DayFourSrc
    ( prepare,
      mkBoard,
      inputBoards,
      parsedBoards,
      pToList,
      parseNums,
      BoardNumber (Bn, marked, number), bToP, drawAndMark
      )

import qualified Data.Map as Map
import qualified Data.Matrix as M

inputPathSamples :: String
inputPathSamples = "test/samples/input3.txt"

samplelistAsString :: [Char]
samplelistAsString = "74951117232014212410161361525122218208193261"

testDayFourSuit :: IO ()
testDayFourSuit = do
    lines <- getLines inputPathSamples
    -- mapM_ print (inputBoards lines) -- (\l -> ',' `elem` l || l == ""
    -- print $ mkBoard . prepare .  map words . take 5 $ (inputBoards lines)
    -- print $ parsedBoards (inputBoards lines)
    -- print $ map mkBoard . parsedBoards $ inputBoards lines
    let mapped = bToP $ parsedBoards $ inputBoards lines
    print $ mapped
    case Map.lookup 16777215 mapped of
     Nothing -> error "Nothing while getting the first input of the Map!"
     Just ma -> print $ M.unsafeGet 1 1 ma
    print $ "Parsing input list..."
    print $ pToList (parseNums lines)
    print $ pToList (parseNums lines) == [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
    print $ "Draw and Mark Tests"
    print $ drawAndMark mapped (pToList (parseNums lines))
    print $ "Semi Group tests"
    -- print $ "t t t - 0"
    -- -- print $ (Bn 1 True <> Bn 2 True) <> Bn 3 True
    -- -- print $ Bn 1 True <> (Bn 2 True <> Bn 3 True)
    -- print $ (Bn 1 True <> Bn 2 True) <> Bn 3 True == Bn 1 True <> (Bn 2 True <> Bn 3 True)
    -- print $ "f f f - 6"
    -- -- print $ (Bn 1 False <> Bn 2 False) <> Bn 3 False
    -- -- print $ Bn 1 False <> (Bn 2 False <> Bn 3 False)
    -- print $ (Bn 1 False <> Bn 2 False) <> Bn 3 False == Bn 1 False <> (Bn 2 False <> Bn 3 False)
    -- print $ "f t t - 1"
    -- -- print $ (Bn 1 False <> Bn 2 True) <> Bn 3 True
    -- -- print $ Bn 1 False <> (Bn 2 True <> Bn 3 True)
    -- print $ (Bn 1 False <> Bn 2 True) <> Bn 3 True == Bn 1 False <> (Bn 2 True <> Bn 3 True)
    -- print $ "t f t - 2"
    -- -- print $ (Bn 1 True <> Bn 2 False) <> Bn 3 True 
    -- -- print $ Bn 1 True <> (Bn 2 False <> Bn 3 True)
    -- print $ (Bn 1 True <> Bn 2 False) <> Bn 3 True == Bn 1 True <> (Bn 2 False <> Bn 3 True)
    -- print $ "t t f - 3"
    -- -- print $ (Bn 1 True <> Bn 2 True) <> Bn 3 False  
    -- -- print $ Bn 1 True <> (Bn 2 True <> Bn 3 False)
    -- print $ (Bn 1 True <> Bn 2 True) <> Bn 3 False == Bn 1 True <> (Bn 2 True <> Bn 3 False)
    -- print $ "f f t - 3"
    -- -- print $ (Bn 1 False <> Bn 2 False) <> Bn 3 True  
    -- -- print $ Bn 1 False <> (Bn 2 False <> Bn 3 True)
    -- print $ (Bn 1 False <> Bn 2 False) <> Bn 3 True == Bn 1 False <> (Bn 2 False <> Bn 3 True)
    -- print $ "t f f - 5"
    -- -- print $ (Bn 1 True <> Bn 2 False) <> Bn 3 False  
    -- -- print $ Bn 1 True <> (Bn 2 False <> Bn 3 False)
    -- print $ (Bn 1 True <> Bn 2 False) <> Bn 3 False == Bn 1 True <> (Bn 2 False <> Bn 3 False)
    -- print $ "f t f - 4"
    -- -- print $ (Bn 1 False <> Bn 2 True) <> Bn 3 False  
    -- -- print $ Bn 1 False <> (Bn 2 True <> Bn 3 False)
    -- print $ (Bn 1 False <> Bn 2 True) <> Bn 3 False == Bn 1 False <> (Bn 2 True <> Bn 3 False) 
    print $ "Ord test"
    print $ Bn 1 False < Bn 2 True
    print $ Bn 1 False > Bn 2 True
    print $ Bn 1 False == Bn 2 True
    print $ "row check"
    let b = M.fromList 3 3 (replicate 9 (Bn 1 False))
    let b = M.fromList 3 3 [Bn 1 True, Bn 1 True, Bn 1 True, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False, Bn 1 False]
    print $ b
    let rowCheck = any (\v ->  and . M.toList . fmap marked . M.submatrix v v 1 3 $ b) [1..3]
    let colCheck = any (\v ->  and . M.toList . fmap marked . M.submatrix 1 3 v v $ b) [1..3]
    print $ rowCheck
    print $ colCheck
    print $ number (foldl (<>) (Bn 0 True) . M.toList $ b)


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

