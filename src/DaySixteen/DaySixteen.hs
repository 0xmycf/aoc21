{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

testInput :: FilePath -> IO [String]
testInput path = mapping >>= \x -> fmap (concat . mapMaybe (`lookup` x)) <$> getLines path

mainDaySixteen :: IO ()
mainDaySixteen = putStrLn "Day Sixteen..." >> problemOne >> problemTwo >> putStrLn "Day Sixteen over.\n "

testDaySixteen :: IO ()
testDaySixteen = do
    putStrLn "Test Day Sixteen..."
    inp <- testInput testPath
    mapM_ (print . parse packetParser) inp
    print . fmap (\case
      Left  pe -> error $ show pe
      Right pa -> operate pa ) $ parse packetParser <$> inp

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
    go i = \case 
      Literal  _     -> i
      Operator _ pas -> i + foldl (\acc p -> go (acc + version p) (typ p)) 0 pas

problemTwo :: IO ()
problemTwo = do
    inp <- input inputPath
    print . (\case
      Left  pe -> error $ show pe
      Right pa -> operate pa ) . parse packetParser $ inp

operate :: Packet -> Int
operate (Packet _ (Literal i))       = i
operate p@(Packet _ (Operator o li)) = case o of
  Sum     -> sum     $ fmap operate li
  Product -> product $ fmap operate li
  Min     -> operate . minimum $ li
  Max     -> operate . maximum $ li 
  GrT     -> greater p
  LeT     -> less    p
  EQT     -> equal   p
  where
    greater, less, equal :: Packet -> Int
    greater (Packet _ (Operator _ [x1, x2])) = if operate x1 > operate x2 then 1 else 0
    less (Packet _ (Operator _ [x1, x2]))    = if operate x1 < operate x2 then 1 else 0
    equal (Packet _ (Operator _ [x1, x2]))   = if operate x1 == operate x2 then 1 else 0

type Version = Int

data OID
    = Sum      -- ^ the sum operator
    | Product  -- ^ the product operator
    | Min      -- ^ the minimum operator
    | Max      -- ^ the maximum operator
    | GrT      -- ^ the greater than operator
    | LeT      -- ^ the less than operator
    | EQT      -- ^ the equal to operator
    deriving (Show, Read, Eq)

data Type
    = Literal  Int
    | Operator OID [Packet]
    deriving (Show, Read)

data Packet = Packet
            { version :: Version
            , typ     :: Type
            } deriving (Show, Read)

-- Part 2 is really easy in Haskell, you just need to give your data some instances
-- Num, Eq, Ord

instance Eq Packet where
  Packet _ (Literal     i) == Packet _ (Literal    j) = i == j
  Packet _ (Operator oo i) == Packet _ (Operator o j) = i == j && oo == o
  _ == _                                              = False

instance Ord Packet where
  Packet _ (Literal i)     `compare` Packet _ (Literal i')     = compare i i'
  Packet _ (Operator _ li) `compare` Packet _ (Operator _ li') = compare li li'
  _ `compare` _              = error "Cannot compare different Operator Types!"

instance Num Packet where
  Packet _ (Literal i)     + Packet _ (Literal ii)    = Packet 0 (Literal $ i + ii)
  Packet _ (Operator _ li) + packet                   = sum li + packet
  packet                   + Packet _ (Operator _ li) = sum li + packet

  Packet _ (Literal i)     * Packet _ (Literal ii)    = Packet 0 (Literal $ i * ii)
  packet                   * Packet _ (Operator _ li) = packet * product li
  Packet _ (Operator _ li) * packet                   = product li * packet

  abs (Packet _ (Literal i))        = Packet 0 (Literal . abs $ i)
  abs (Packet _ (Operator o li))    = Packet 0 (Operator o $ map abs li)

  signum (Packet _ (Literal i))     = Packet 0 (Literal . signum $ i)
  signum (Packet _ (Operator o li)) = Packet 0 (Operator o $ map signum li)

  fromInteger i                     = Packet 0 . Literal . fromInteger $ i

  negate (Packet _ (Literal i))     = Packet 0 (Literal . negate $ i)
  negate (Packet _ (Operator o li)) = Packet 0 (Operator o $ map negate li)

packetParser :: ParsecT String u Identity Packet
packetParser = Packet <$> versionParser <*> typeParser

versionParser :: ParsecT String u Identity Version
versionParser = do
    c1 <- P.anyChar
    c2 <- P.anyChar
    c3 <- P.anyChar
    pure $ binToDec [c1, c2, c3]

numToOID :: Int -> OID
numToOID 0 = Sum
numToOID 1 = Product
numToOID 2 = Min
numToOID 3 = Max
numToOID 5 = GrT
numToOID 6 = LeT
numToOID 7 = EQT
numToOID i = error $ "Couldn't parse int to OID, with num" ++ show i

typeParser :: ParsecT String u Identity Type
typeParser = do
    c1 <- P.anyChar
    c2 <- P.anyChar
    c3 <- P.anyChar
    if binToDec [c1, c2, c3] == 4
        then Literal . binToDec <$> groupParser
        else Operator (numToOID . binToDec $ [c1, c2, c3]) <$> do
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

{-
    Packet:
    VVV--TTT--
    The last trailing 0 bits are not part of the number
    where
        V is the Version
        T is the Type ID (4 is literal value, else operator)
        where
            literal values encode a single binary number
                    its length is always a multiple of 4
            where
                number consists of groups
                groups start either with 1 or if its the last with 0,
                            these are not counted into the equation
                        The remaining bits represent the binary number.
            operator contains packets
                     Have a 'length type id' after the 'VVV--TTT--V'
                     where
                        'length type id'    starts with either 1 or 0
                                            1 11 next bits represent the number of sub-packets immediately contained
                                            0 15 next bits represent the total length in bits
                     then they contain sub-packets which are structured the same way
-}

