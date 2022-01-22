{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import           Data.Maybe               (fromMaybe)
import           Text.Read                (readMaybe)

import           DayEight.DayEight        (mainDayEight, testDayEight)
import           DayFive.DayFive          (mainDayFive, testDayFive)
import           DayFour.DayFour          (mainDayFour, testDayFour)
import           DayNine.DayNine          (mainDayNine, testDayNine)
import           DayOne.First.DayOneOne   (mainDayOne, testDayOne)
import           DaySeven.DaySeven        (mainDaySeven, testDaySeven)
import           DaySix.DaySix            (mainDaySix, testDaySix)
import           DayThree.DayThree        (mainDayThree, testDayThree)
import           DayTwo.First.DayTwoFirst (mainDayTwo, testDayTwo)

import           Advent                   (Part (Part1))
import           Common.Days              (inputBlock, promptBlock, submitBlock)
import qualified Common.Days              as Days
import           DayFifteen.DayFifteen    (mainDayFifteen, testDayFifteen)
import           System.Exit              (exitSuccess)

main :: IO ()
main = do
    putStrLn "test/run/{day}/{day}t/prompt/input/bm/new/dl/submit/exit"
    input <- getLine
    case input of
      "test"    -> testDayOne >> testDayTwo >> testDayThree >> testDayFour >> testDayFive >> testDaySix >> testDaySeven >> testDayEight >> testDayNine
      "run"     -> mainDayOne >> mainDayTwo >> mainDayThree >> mainDayFour >> mainDayFive >> testDaySix >> mainDaySeven >> mainDayEight >> mainDayNine
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
      "9"       -> mainDayNine      -- main 9
      "9t"      -> testDayNine
      "15"      -> mainDayFifteen   -- main 15
      "15t"     -> testDayFifteen
      "prompt"  -> let day = print "Please enter a Day Number" *> (fromMaybe 1 . readMaybe <$> getLine) in promptBlock day
      "input"   -> let day = print "Please enter a Day Number" *> (fromMaybe 1 . readMaybe <$> getLine) in inputBlock day
      "submit"  -> let day = print "Please enter a Day Number" *> (fromMaybe 1 . readMaybe <$> getLine)
                       part= print "Please enter Part (Part1 | Part2)" *> (fromMaybe Part1 . readMaybe <$> getLine)
                       answer = print "Please enter your answer to " *> getLine in
                         submitBlock day part answer
      "dl"      -> let day = print "Please enter a Day Number" *> (fromMaybe 1 . readMaybe <$> getLine) in promptBlock day >> inputBlock day
      "bm"      -> print "Please use 'stack bench' for that!"
      "new"     -> let bl = putStrLn "Are you sure you want to create a new day? (True/False)" *> (fromMaybe False . readMaybe <$> getLine) in do
                            bl >>= (\x -> if x then Days.initDay else putStrLn "" *> main)
      "e"       -> exitSuccess
      "exit"    -> exitSuccess
      "q"       -> exitSuccess
      "quit"    -> exitSuccess
      _         -> print ("Please enter test or run or num(t). Available Nums are: " ++ show [1..10]) *> main
