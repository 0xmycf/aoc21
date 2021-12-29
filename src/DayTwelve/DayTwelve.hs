module DayTwelve.DayTwelve
( mainDayTwelve
, testDayTwelve
) where

import           Common.Lib            (getLines, parse)
import           Data.Functor.Identity (Identity)
import           Data.Map              (Map)
import           Text.Parsec           (ParseError, ParsecT, (<|>))
import qualified Data.Map              as Map
import qualified Text.Parsec           as Parsec

data Cave
    = Start
    | End
    | Small String Bool -- name , if the cave has already been visited or not
    | Large String      -- name
    deriving (Show, Read, Eq, Ord)


inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayTwelve.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayTwelve.txt"

-- | parses the input depending on the FilePath given
input :: [Char] -> IO (Either ParseError [(Cave, Cave)])
input path = let input = getLines path
             in mapM (parse lineParser) <$> input

mainDayTwelve :: IO ()
mainDayTwelve = putStrLn "Day Twelve..." >> problemOne >> problemTwo >> putStrLn "Day Twelve over.\n "

testDayTwelve :: IO ()
testDayTwelve = do
    putStrLn "Test Day Twelve..."
    putStrLn "Test Day Twelve over.\n"


problemOne :: IO ()
problemOne = print "to be impl"

problemTwo :: IO ()
problemTwo = print "to be impl"


{-
    Map from each node to the nodes it can reach
-}

adj :: [(Cave, Cave)] -> Map Cave [Cave]
adj xs = Map.fromListWith (++) (fmap (fmap (: [])) xs)


{-
    Parsing
-}

sep:: ParsecT String u Identity ()
sep = Parsec.spaces *> Parsec.char '-' *> Parsec.spaces

smallCave :: ParsecT String u Identity Cave
smallCave = Small <$> Parsec.many1 Parsec.lower <*> pure False

largeCave :: ParsecT String u Identity Cave
largeCave = Large <$> Parsec.many1 Parsec.upper

startCave :: ParsecT String u Identity Cave
startCave = Start <$ Parsec.string "start"

endCave :: ParsecT String u Identity Cave
endCave = End <$ Parsec.string "end"

anyCave :: ParsecT String u Identity Cave
anyCave = startCave <|> endCave <|> smallCave <|> largeCave

lineParser :: ParsecT String u Identity (Cave, Cave)
lineParser = do
    cave1 <- anyCave
    sep
    cave2 <- anyCave
    pure (cave1, cave2)
