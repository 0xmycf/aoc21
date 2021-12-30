{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DayTwelve.DayTwelve
( mainDayTwelve
, testDayTwelve
) where

import           Common.Lib            (getLines, parse)
import           Data.Functor.Identity (Identity)
import           Data.List             (nub)
import           Data.Map              (Map)
import           Data.Maybe            (isNothing)
import           Data.Set              (Set)
import           Data.Tuple            (swap)
import           Text.Parsec           (ParseError, ParsecT, (<|>))
import qualified Data.Set              as Set
import qualified Data.Map              as Map
import qualified Text.Parsec           as Parsec

data Cave
    = Start
    | End
    | Small String      -- name
    | Large String      -- name
    deriving (Show, Read, Eq, Ord)

type AdjacencyMap = Map Cave (Set Cave)


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
    inp <- input testPath
    case inp of
      Left  pe -> print pe
      Right cavesys -> let cvsys = adj cavesys
                           filter' = \newCaves v -> v `notElem` newCaves || isLarge v in do
          print $ getAPath filter' cvsys (Large "A")
          print $ getAPath filter' cvsys (Large "FOO")
          print $ getAPath filter' cvsys Start
    putStrLn "Test Day Twelve over.\n"

problemOne :: IO ()
problemOne = input inputPath >>= print . (\case
   Left  _  -> 0
   Right x0 -> let ads = adj x0 in length $ getAPath (\newCaves v -> v `notElem` newCaves || isLarge v) ads Start)

problemTwo :: IO () -- takes too long
problemTwo = input inputPath >>= print . (\case
   Left  _  -> error "Parsing error"
   Right x0 ->
       let ads = adj x0
        in length $ getAPath (\newCaves v -> v `notElem` newCaves || isLarge v || isOnce' newCaves) ads Start)
        --in length $ getAPath (\newCaves v -> v `notElem` newCaves || isLarge v || isOnce (frequencyMap newCaves) v) ads Start)

-- | I found this filtering method on Reddit, but it doesn't seem to be faster
isOnce' :: [Cave] -> Bool
isOnce' caves = length (nub smallCaves) == length smallCaves
    where
    smallCaves = filter isSmall caves

isOnce :: Map Cave Int -> Cave -> Bool
isOnce freq cave
    | isNothing (cave `Map.lookup` freq)   = True
    | isSmall cave && freq Map.! cave > 1  = False
    | otherwise = let (small, _) = Map.partitionWithKey (\k v -> isSmall k && v > 1) freq
                  in null small

-- | Map from each node to the nodes it can reach
-- | GHC can infer a much more general type than what I gave it
-- adj :: Ord k => [(k, k)] -> Map k (Set k)
adj :: [(Cave, Cave)] -> AdjacencyMap
adj xs = let xs' = xs ++ fmap swap xs in Map.fromListWith Set.union (fmap (fmap Set.singleton) xs')

-- | gets all paths from startCave given to an end cave, if such exists,
-- | depending on the predicate f
getAPath :: ([Cave] -> Cave -> Bool) -> AdjacencyMap -> Cave -> [[Cave]]
getAPath f mp cave
    | isNothing (cave `Map.lookup` mp) = []
    | isNothing (End  `Map.lookup` mp) = []
    | otherwise = go [cave] (Set.toList $ mp Map.! cave)
    where
    go :: [Cave] -> [Cave] -> [[Cave]]
    go _     []      = []
    go caves (x:xs)
        | x == cave  = go caves xs
        | x == Start = go caves xs
        | isEnd x    = newCaves : go caves xs
        | otherwise  = go newCaves (filter' . Set.toList $ mp Map.! x) ++ go caves xs
        where
        newCaves = x : caves
        filter'  = filter (f newCaves)


isEnd :: Cave -> Bool
isEnd End = True
isEnd _   = False

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _         = False

isLarge :: Cave -> Bool
isLarge = not . isSmall


{-
    Parsing
-}

sep :: ParsecT String u Identity ()
sep = Parsec.spaces *> Parsec.char '-' *> Parsec.spaces

smallCave :: ParsecT String u Identity Cave
smallCave = Small <$> Parsec.many1 Parsec.lower

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
