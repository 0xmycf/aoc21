{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module DaySixteen.DaySixteen
( mainDaySixteen
, testDaySixteen
) where

import           Common.Lib            (binToDec, getLines, parse)
import           Data.Functor.Identity (Identity)
import           Data.Map              (Map, lookup)
import qualified Data.Map              as Map
import           Data.Maybe            (mapMaybe)
import           Prelude               hiding (lookup)
import           Text.Parsec           (ParsecT)
import qualified Text.Parsec           as P

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
    print "for 110100101111111000101000"
    print . parse packetParser $ "110100101111111000101000"
    putStrLn "Test Day Sixteen over.\n"


problemOne :: IO ()
problemOne = do
    inp <- input inputPath
    print . parse packetParser $ inp
    print . (\case
      Left  pe -> error $ show pe
      Right pa -> sumNums pa
      ) . parse packetParser $ inp

sumNums :: Packet -> Int
sumNums packet = go (version packet) (typ packet)
    where
    go :: Int -> Type -> Int
    go i t = case t of
      Literal  _   -> i
      Operator pas -> i + foldl (\acc p -> go (acc + version p) (typ p)) 0 pas

problemTwo :: IO ()
problemTwo = print "to be impl"

type Version = Int
type Length  = Int

data Type
    = Literal  Int
    | Operator [Packet]
    deriving (Show, Read)

data Packet = Packet
            { version :: Version
            , typ     :: Type
            } deriving (Show, Read)


packetParser :: ParsecT String u Identity Packet
packetParser = Packet <$> versionParser <*> typeParser

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
        then Literal . binToDec <$> groupParser
        else Operator <$> do
            len <- P.anyChar
            if len == '1'
                then elevenOperator          -- 1 11 next bits represent the number of sub-packets immediately contained
                else fifteenOperator         -- 0 15 next bits represent the total length in bits

nCharsParser :: Int -> ParsecT String u Identity String
nCharsParser int = go int (pure "")
    where
    go i str
        | i == 0    = str
        | otherwise = do
            c <- P.anyChar
            go (i-1) ((++[c]) <$> str)

elevenOperator :: ParsecT String u Identity [Packet]
elevenOperator = do
    str <- nCharsParser 11
    nPacketsParser (binToDec str)

fifteenOperator :: ParsecT String u Identity [Packet]
fifteenOperator = do
    str     <- nCharsParser 15
    packets <- nCharsParser (binToDec str)
    let p = parse (P.many1 packetParser) packets
    case p of
      Left  pe -> error . show $ pe
      Right pa -> pure pa

nPacketsParser :: Int -> ParsecT String u Identity [Packet]
nPacketsParser int = go int (pure [])
    where
    go i p
        | i == 0 = p
        | otherwise = do
            p' <- packetParser
            go (i-1) ((++[p']) <$> p)

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

    where

