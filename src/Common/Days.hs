{-# OPTIONS_GHC -Wno-missing-fields #-}
-- |
-- | A module that holds
-- | scripts and functions
-- | for setup etc.
-- |
------------------------
module Common.Days
( initDay
, getPrompt
, getInput
, promptBlock
, inputBlock
, submit
, submitBlock
) where

import           Data.Text        (Text)

import           Advent           (AoC (AoCInput, AoCPrompt, AoCSubmit),
                                   AoCError,
                                   AoCOpts (AoCOpts, _aCache, _aForce, _aSessionKey, _aThrottle, _aYear),
                                   Part, SubmitRes, mkDay_, runAoC)
import           Control.Monad    ((>=>))
import qualified Data.Map         as Map
import           System.Directory (createDirectoryIfMissing, doesFileExist,
                                   renameFile)
import           System.Exit      (exitFailure)

type Answer = String

data DayNumber
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Eleven
    | Twelve
    | Thirteen
    | Fourteen
    | Fifteen
    | Sixteen
    | Seventeen
    | Eighteen
    | Nineteen
    | Twenty
    | TwentyOne
    | TwentyTwo
    | TwentyThree
    | TwentyFour deriving (Show, Read, Enum, Eq)

initDay :: IO ()
initDay = do
    day <- putStrLn "Please enter the day number" *> getLine -- might change to automatically know what day I am missing
    (dayNum, fileTemplate) <- parseDayFile (read day) <$> readFile "./template/DayX.tmp"
    doesFileExist ("./src/Day" ++ show dayNum ++ "/" ++ "Day" ++ show dayNum ++ ".hs") >>= \bl ->
        if bl
        then putStrLn "The file already exists, abandoning..." *> exitFailure
        else do
            createDirectoryIfMissing True ("./src/Day" ++ show dayNum ++ "/")
            writeFile ("./src/Day" ++ show dayNum ++ "/" ++ "Day" ++ show dayNum ++ ".hs") fileTemplate
            writeFile ("./inputs/test/Day" ++ show dayNum ++ ".txt") "test input"
            let dayAsInteger = pure . toInteger .  fromEnum $ dayNum
            promptBlock dayAsInteger *> inputBlock dayAsInteger
            dayAsInteger >>= \x ->
                renameFile ("inputs/auto/input/2021/" ++ show x ++ ".txt") ("inputs/auto/input/2021/Day" ++ show dayNum ++ ".txt")


parseDayFile :: Int -> String -> (DayNumber, String)
parseDayFile i str = let ei = (toEnum i :: DayNumber) in (ei , foldl (\acc x -> if x == 'X' then acc ++ show ei else acc ++ [x]) "" str)

getPrompt :: Integer -> IO (Either Advent.AoCError (Map.Map Advent.Part Text))
getPrompt day = defaultOpts >>= \x -> runAoC x $ AoCPrompt (mkDay_ day)

getInput :: Integer -> IO (Either AoCError Text)
getInput day = inputOpts >>= \x -> runAoC x $ AoCInput (mkDay_ day)

submit :: Integer -> Part -> Answer -> IO (Either AoCError (Text, SubmitRes))
submit day part answer = inputOpts >>= \x -> runAoC x $ AoCSubmit (mkDay_ day) part answer

inputOpts :: IO AoCOpts
inputOpts = do
    cookie <- getSessionCookie
    pure AoCOpts {
    _aSessionKey = cookie,
    _aYear = 2021,
    _aCache = Just "inputs/auto",
    _aForce = False,
    _aThrottle = 3000000
}

defaultOpts :: IO AoCOpts
defaultOpts = do
    cookie <- getSessionCookie
    pure AoCOpts {
    _aSessionKey = cookie,
    _aYear = 2021,
    _aCache = Just "/Users/mycf/Documents/haskell/AOC21STACK/AOC21/cache",
    _aForce = False,
    _aThrottle = 3000000
}

getSessionCookie :: IO String
getSessionCookie = readFile "config.txt"


promptBlock :: IO Integer -> IO ()
promptBlock day = day >>= (getPrompt >=> \case
                        Left ace  -> print ace *> exitFailure
                        Right _   -> print "The prompt has been downloaded to cache/prompt/2021/")

inputBlock :: IO Integer -> IO ()
inputBlock day = day >>= (getInput >=> \case
                              Left ace  -> print ace *> exitFailure
                              Right _   -> print "The input has been downloaded to inputs/auto/input/2021/")

submitBlock :: IO Integer -> IO Part -> IO Answer -> IO ()
submitBlock day part ans = do
                            d <- day
                            p <- part
                            a <- ans
                            s <- submit d p a
                            case s of
                              Left  ace    -> print ace *> exitFailure
                              Right (a',b) -> print a' *> print "" *> print b
