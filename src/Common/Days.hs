-- | 
-- | A module that holds 
-- | scripts and functions 
-- | for setup etc.
-- |
------------------------
module Common.Days
( initDay
) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import System.Environment (getExecutablePath)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getDirectoryContents, doesFileExist)

import qualified Text.Parsec as P
import System.Exit (exitFailure)

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
    doesFileExist ("./src/Day" ++ dayNum' ++ "/" ++ "Day" ++ dayNum' ++ ".hs") >>= (\bl ->
        if bl
        then putStrLn "The file already exists, abandoning..." *> exitFailure
        else 
            createDirectoryIfMissing True ("./src/Day" ++ dayNum' ++ "/") *>
            writeFile ("./src/Day" ++ dayNum' ++ "/" ++ "Day" ++ dayNum' ++ ".hs") fileTemplate
        )

parseDayFile :: Maybe Int -> String -> (Maybe DayNumber, String)
parseDayFile i str = let ei = (toEnum <$> i :: Maybe DayNumber) in (ei , foldl (\acc x -> if x == 'X' then acc ++ show (fromMaybe NoDay ei) else acc ++ [x]) "" str)
