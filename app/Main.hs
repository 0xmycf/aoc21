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

import           Common.Days              (getInput, getPrompt)
import qualified Common.Days              as Days
import           System.Exit              (exitFailure)
import Control.Monad ((>=>))

main :: IO ()
main = do
    putStrLn "test/run/{day}/{day}t/prompt/input/bm/new"
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
      "prompt"  -> let day = print "Please enter a Day Number" *> (fromMaybe 1 . readMaybe <$> getLine) in
                            day >>= (getPrompt >=> \case
                                      Left ace  -> print ace *> exitFailure 
                                      Right _   -> print "The input has been downloaded to cache/prompt/2021/")
      "input"   -> let day = print "Please enter a Day Number" *> (fromMaybe 1 . readMaybe <$> getLine) in
                            day >>= (getInput >=> \case
                                      Left ace  -> print ace *> exitFailure 
                                      Right _   -> print "The input has been downloaded to inputs/auto/input/2021/")
      "bm"      -> print "Please use 'stack bench' for that!"
      "new"     -> let bl = putStrLn "Are you sure you want to create a new day? (True/False)" *> (fromMaybe False . readMaybe <$> getLine) in
                            bl >>= (\x -> if x then Days.initDay else putStrLn "" *> main)
      _         -> error $ "Please enter test or run or num(t). Available Nums are: " ++ show [1..9]
