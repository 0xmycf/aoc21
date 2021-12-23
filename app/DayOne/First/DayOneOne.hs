{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DayOne.First.DayOneOne
( mainDayOne
, testDayOne
) 
where

exin :: [Int]
exin = [1,2,3,4,5,1,2,3,6]

{- 

For the first question
    1 no-change, 2 increase, 3 increase, 4 increase, 5 increase, 1 decrease, 2 increase 
should return 5

For the second question
    1,2,3=6, 2,3,4=9, 3,4,5=12, 4,5,1=10, 5,1,2=8, 1,2,3=6, 2,3,6=11
    6 no-change, 9 increase, 12 increase, 10 decrease, 8 decrease, 6 decrease, 11 increase
should return 3

-}

testDayOne :: IO ()
testDayOne = do
    putStrLn "Day One Tests..."
    list <- getLines "./app/DayOne/First/input.txt"
    mapM_ print list
    putStrLn "Day One Tests over.\n"

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

mainDayOne :: IO ()
mainDayOne = do
    putStrLn "Day One..."
    list <- getLines "./app/DayOne/First/input.txt"
    let ilist = mapThreeTogether [] $ map read list
    print $ getAns $ getDifference ilist (head ilist) []
    putStrLn "Day One over.\n"


whatChange :: Int -> String
whatChange i
    | i >  0    = "increase"
    | i <  0    = "decrease"
    | i == 0    = "no change"
    | otherwise = error "Something went wrong!"

getDifference :: [Int] -> Int -> [(Int,String)] -> [(Int,String)]
getDifference [] _ acc      = acc
getDifference (x:xs) i acc  = getDifference xs x ((x, whatChange $ x - i) : acc)

getAns :: [(Int, String)] -> Int
getAns = foldl (\ p c -> p + isIncrease c "increase") 0
    where
        isIncrease :: (Int, String) -> String -> Int
        isIncrease (_,s) str
            | s == str = 1
            | otherwise = 0

mapThreeTogether :: [Int] -> [Int] -> [Int]
mapThreeTogether acc []             = reverse acc
mapThreeTogether acc (x1:x2:x3:xs)  = mapThreeTogether ((x1 + x2 + x3) : acc) (x2 : x3 : xs)
mapThreeTogether acc (x1:x2:xs)     = mapThreeTogether ((x1 + x2) : acc) (x2 : xs)    
mapThreeTogether acc (x1:xs)        = mapThreeTogether (x1 : acc) xs