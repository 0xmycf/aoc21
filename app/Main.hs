module Main where
import DayOne.First.DayOneOne (mainDayOne, testDayOne)
import DayTwo.First.DayTwoFirst (mainDayTwo, testDayTwo)
import DayThree.DayThree ( mainDayThree, testDayThree )
import DayFour.DayFour (mainDayFour, testDayFour)

main :: IO ()
main = do
    putStrLn "test/run"
    input <- getLine
    case input of 
      "test"    -> testDayOne >> testDayTwo >> testDayThree
      "run"     -> mainDayOne >> mainDayTwo >> mainDayThree
      "1"       -> mainDayOne       -- main 1
      "1t"      -> testDayOne  
      "2"       -> mainDayTwo       -- main 2
      "2t"      -> testDayTwo  
      "3"       -> mainDayThree     -- main 3
      "3t"      -> testDayThree  
      "4"       -> mainDayFour      -- main 4
      "4t"      -> testDayFour   
      _         -> error "Please enter test or run or num(t)"
