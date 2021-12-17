module Main where
  
import DayOne.First.DayOneOne   ( mainDayOne  , testDayOne  )
import DayTwo.First.DayTwoFirst ( mainDayTwo  , testDayTwo  )
import DayThree.DayThree        ( mainDayThree, testDayThree)
import DayFour.DayFour          ( mainDayFour , testDayFour )
import DayFive.DayFive          ( mainDayFive , testDayFive )
import DaySix.DaySix            ( mainDaySix  , testDaySix  )
import DaySeven.DaySeven        ( mainDaySeven, testDaySeven)

main :: IO ()
main = do
    putStrLn "test/run"
    input <- getLine
    case input of 
      "test"    -> testDayOne >> testDayTwo >> testDayThree >> testDayFour >> testDayFive >> testDaySix
      "run"     -> mainDayOne >> mainDayTwo >> mainDayThree >> mainDayFour >> mainDayFive >> testDaySix
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
      "7"       -> mainDaySeven       -- main 6
      "7t"      -> testDaySeven  
      "bm"      -> print   "Please use 'stack bench' for that!"
      _         -> error $ "Please enter test or run or num(t). Available Nums are: " ++ show [1..6]