{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DaySixteen.DaySixteen
( mainDaySixteen
, testDaySixteen
) where

import           Common.Lib  (getLines, binToDec)
import           Data.Map    (Map, lookup)
import qualified Data.Map    as Map
import           Data.Maybe  (mapMaybe)
import           Prelude     hiding (lookup)
import           Text.Parsec (ParseError, ParsecT)
import qualified Text.Parsec as P
import Data.Functor.Identity (Identity)

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DaySixteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DaySixteen.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DaySixteen.txt"

mapping :: IO (Map Char String)
mapping = do
    inp <- getLines "inputs/DaySixteenMapping.txt"
    pure $ Map.fromList $
        fmap toTuple inp
    where
    toTuple :: String -> (Char, String)
    toTuple (a:xs) = (a, xs)
    toTuple _      = ('Z', "")

-- | parses the input depending on the FilePath given
input :: FilePath -> IO String
input path = mapping >>= \x -> concat . mapMaybe (`lookup` x) <$> readFile path

mainDaySixteen :: IO ()
mainDaySixteen = putStrLn "Day Sixteen..." >> problemOne >> problemTwo >> putStrLn "Day Sixteen over.\n "

testDaySixteen :: IO ()
testDaySixteen = do
    putStrLn "Test Day Sixteen..."
    putStrLn "Test Day Sixteen over.\n"


problemOne :: IO ()
problemOne = print "to be impl"

problemTwo :: IO ()
problemTwo = print "to be impl"

type Version  = Int 

data Type 
    = Literal  Int
    | Operator [Packet]
    deriving (Show, Read)

data Packet = Packet
            { version :: Version
            , typ     :: Type
            } deriving (Show, Read)


packetParser :: ParsecT String u Identity Packet
packetParser = undefined

versionParser :: ParsecT String u Identity Version
versionParser = do
    c1 <- P.anyChar 
    c2 <- P.anyChar
    c3 <- P.anyChar
    pure $ binToDec [c1, c2, c3]

typeParser :: ParsecT String u Identity Type
typeParser = do
    c1 <- P.anyChar 
    c2 <- P.anyChar
    c3 <- P.anyChar
    if binToDec [c1, c2, c3] == 4
        then pure $ Literal (binToDec <$> groupParser)
        else pure $ Operator do
            error "Missing [Packet] Parser"
    where
    groupParser :: ParsecT String u Identity String
    groupParser = do
        c1 <- P.anyChar 
        if c1 == '1'
            then do
                fours <- takeFour
                (fours ++) <$> groupParser
            else takeFour 
    takeFour :: ParsecT String u Identity String
    takeFour = do
        c1 <- P.anyChar 
        c2 <- P.anyChar 
        c3 <- P.anyChar 
        c4 <- P.anyChar 
        pure [c1, c2, c3, c4]

