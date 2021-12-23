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
) where

import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text)
import           Text.Read        (readMaybe)

import           Advent           (AoC (AoCInput, AoCPrompt), AoCError,
                                   AoCOpts (AoCOpts, _aCache, _aForce, _aSessionKey, _aThrottle, _aYear),
                                   Part, mkDay_, runAoC)
import qualified Data.Map         as Map
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.Exit      (exitFailure)

data DayNumber
    = NoDay
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
    | TwentyFour deriving (Show, Enum, Eq)

initDay :: IO ()
initDay = do
    day <- putStrLn "Please enter the day number" *> getLine -- might change to automatically know what day I am missing
    (dayNum, fileTemplate) <- parseDayFile (readMaybe day) <$> readFile "./template/DayX.tmp"
    let dayNum' = show (fromMaybe NoDay dayNum)
    doesFileExist ("./src/Day" ++ dayNum' ++ "/" ++ "Day" ++ dayNum' ++ ".hs") >>= \bl ->
        if bl
        then putStrLn "The file already exists, abandoning..." *> exitFailure
        else
            createDirectoryIfMissing True ("./src/Day" ++ dayNum' ++ "/") *>
            writeFile ("./src/Day" ++ dayNum' ++ "/" ++ "Day" ++ dayNum' ++ ".hs") fileTemplate


parseDayFile :: Maybe Int -> String -> (Maybe DayNumber, String)
parseDayFile i str = let ei = (toEnum <$> i :: Maybe DayNumber) in (ei , foldl (\acc x -> if x == 'X' then acc ++ show (fromMaybe NoDay ei) else acc ++ [x]) "" str)

getPrompt :: Integer -> IO (Either Advent.AoCError (Map.Map Advent.Part Text))
getPrompt day = defaultOpts >>= (\x -> runAoC x $ AoCPrompt (mkDay_ day))

getInput :: Integer -> IO (Either AoCError Text)
getInput day = inputOpts >>= (\x -> runAoC x $ AoCInput (mkDay_ day))

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
