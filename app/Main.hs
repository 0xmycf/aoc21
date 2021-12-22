module Main where
  
import Text.Read   (readMaybe)
import Data.Maybe  (fromMaybe)

import DayOne.First.DayOneOne   ( mainDayOne  , testDayOne  )
import DayTwo.First.DayTwoFirst ( mainDayTwo  , testDayTwo  )
import DayThree.DayThree        ( mainDayThree, testDayThree)
import DayFour.DayFour          ( mainDayFour , testDayFour )
import DayFive.DayFive          ( mainDayFive , testDayFive )
import DaySix.DaySix            ( mainDaySix  , testDaySix  )
import DaySeven.DaySeven        ( mainDaySeven, testDaySeven)
import DayEight.DayEight        ( mainDayEight, testDayEight)
import DayNine.DayNine          ( mainDayNine , testDayNine )

import qualified Common.Days as Days

main :: IO ()
main = do
    putStrLn "test/run"
    input <- getLine
    case input of 
      "test"    -> testDayOne >> testDayTwo >> testDayThree >> testDayFour >> testDayFive >> testDaySix >> testDaySeven
      "run"     -> mainDayOne >> mainDayTwo >> mainDayThree >> mainDayFour >> mainDayFive >> testDaySix >> mainDaySeven
      "1"       -> mainDayOne       -- main 1
      "1t"      -> testDayOne  
      "2"       -> mainDayTwo       -- main 2
      "2t"      -> testDayTwo  
      "3"       -> mainDayThree     -- main 3
      "3t"      -> testDayThree  
      "4"       -> mainDayFour      -- main 4
      "4t"      -> testDayFour   
      "5"       -> mainDayFive      -- main 5
      "5t"      -> testDayFive   
      "6"       -> mainDaySix       -- main 6
      "6t"      -> testDaySix   
      "7"       -> mainDaySeven     -- main 7
      "7t"      -> testDaySeven  
      "8"       -> mainDayEight     -- main 8
      "8t"      -> testDayEight   
      "9"       -> mainDayNine     -- main 9
      "9t"      -> testDayNine   
      "bm"      -> print   "Please use 'stack bench' for that!"
      "new"     -> let bl = putStrLn "Are you sure you want to create a new day? (True/False)" *> (fromMaybe False . readMaybe <$> getLine) in 
                            bl >>= (\x -> if x then Days.initDay else putStrLn "" *> main)
      _         -> error $ "Please enter test or run or num(t). Available Nums are: " ++ show [1..9]