module DayFive.DayFive
( mainDayFive
, testDayFive
) where

import DayFiveSrc (parse)

import qualified DayFiveSrcPartTwo as DayFive
import qualified Data.Map.Strict   as MapS

inputPath :: String
inputPath = "./app/DayFive/input.txt"

mainDayFive :: IO ()
mainDayFive = putStrLn "Day Five..." >> problemOneTwo >> problemTwo >> putStrLn "Day Five over.\n "

testDayFive :: IO ()
testDayFive = do
    putStrLn "Test Day Five..."
    putStrLn "Test Day Five over.\n"

problemOneTwo :: IO ()
problemOneTwo = do
    file <- readFile "app/DayFive/input.txt"
    case parse DayFive.mapParseToPair file of
        Left pe   -> print pe
        Right lis -> print $ length . filter (/=1) . MapS.elems . DayFive.mv2ToInt . DayFive.getVertAndHoriz $ lis

problemTwo :: IO () -- 15203 too low... There must be a mistake in my code...
problemTwo = do
    file <- readFile "app/DayFive/input.txt"
    case parse DayFive.mapParseToPair file of
        Left pe   -> print pe
        Right lis -> print $ length . filter (/=1) . MapS.elems . DayFive.mv2ToInt $ lis







-- My previous tires... Ill keep them as a reminder...
--
-- Problem One 
--
-- file <- readFile inputPath -- "/Users/mycf/Documents/haskell/AOC21STACK/AOC21/test/samples/input5.txt"
-- case parse mapParseToPair file of
--   Left  pe  -> print pe
--   Right lis -> let vhs = getVhs lis in do
--       print $ "length lis: " ++ show (length lis) ++ " length Vhs: " ++ show (length vhs)
--       let lines = map mkLine vhs
--       print $ calc Set.empty vhs
--       print $ length $ calc Set.empty vhs
--         where
--             calc acc []      = Set.fromList . concat $ acc 
--             calc acc (l1:xs) = calc (Set.insert (concatMap (\l2-> Set.elems $ l1 `intersectP` l2) xs) acc) xs
--
-- Problem Two 
--
--     file <- readFile "/Users/mycf/Documents/haskell/AOC21STACK/AOC21/test/samples/input5.txt"
--     case parse mapParseToPair file of
--       Left  pe  -> print pe
--       Right lis -> do
--           -- print $ "length lis: " ++ show (length lis) ++ " length Vhs: " ++ show (length vhs)
--           let lines = map mkLine lis
--           print $ map mkLine lis
--           print $ calc Set.empty lis
--           print $ length $ calc Set.empty lis
--             where
--                 calc acc []      = Set.fromList . concat $ acc 
--                 calc acc (l1:xs) = calc (Set.insert (concatMap (\l2-> Set.elems $ l1 `intersectP` l2) xs) acc) xs


-- problemTwo2 :: IO ()
-- problemTwo2 = do
--     file <- readFile inputPath
--     case parse mapParseToPair file of
--         Left pe   -> print pe
--         Right lis -> let grid = Map.empty in do
--             print $ map mkLine lis
--             --map (\g -> let Map.insert grid) lis
--             print $ "foo"

            