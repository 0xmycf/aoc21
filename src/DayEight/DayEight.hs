module DayEight.DayEight
( mainDayEight
, testDayEight
) where

import Common.Lib (getLines, parse)
import Data.Functor.Identity (Identity)
import Data.Sequence (sortOn)
import Data.Foldable (toList)

import Text.Parsec ((<|>))
import qualified Text.Parsec   as Parsec

import Data.Map (Map)
import qualified Data.Map      as Map

import qualified Data.Sequence as Seq
import Data.List (groupBy)

import qualified Data.Set      as Set
import Data.Set (Set)

inputPath :: String
inputPath = "./inputs/DayEight.txt"

testPath :: String
testPath  = "./inputs/test/DayEight.txt"

mainDayEight :: IO ()
mainDayEight = putStrLn "Day Eight..." >> problemOne >> problemTwo >> putStrLn "Day Eight over.\n "

testDayEight :: IO ()
testDayEight = do
    putStrLn "Test Day Eight..."
    stuff <- mapM (parse dayEightP') <$> getLines inputPath
    case stuff of
        Right r -> print r
        Left  l -> print l
    putStrLn "Test Day Eight over.\n"

decryption :: Map Char Char
decryption = let panels = ['a'..'g'] in Map.fromList (panels `zip` panels)

outputs :: IO [String]
outputs = fmap (fmap (drop 2 . dropWhile (/= '|'))) . getLines $ inputPath

inputs :: IO [String]
inputs = fmap (fmap (takeWhile (/= '|'))) . getLines $ inputPath

problemOne :: IO ()
problemOne = do
    pInput <- mapM (parse dayEightP') <$> getLines inputPath
    case pInput of
      Left  pe -> print pe
      Right x  -> do
          print $ sum . map (foldl (\acc x1 -> let lx = length x1 in if lx == 4 || lx == 2 || lx == 7 || lx == 3 then acc + 1 else acc) 0 . snd) $ x


problemTwo :: IO ()
problemTwo = do
    pInput <- mapM (parse dayEightP') <$> getLines inputPath
    case pInput of
      Left  pe -> print pe
      Right x  -> do -- x :: [([String], [String])]
            let first  = fmap (fmap Set.fromList . filter (/=[]) . fmap (filter (/="")) . groupBy (\a b -> length a == length b) . toList . sortOn length . Seq.fromList . fst) x
                second = fmap snd x
            print "sorry... Until i figure out a smarter way I wont be doing this... Otherwise its just set differences..."
            -- This wont work because I blindly assumed the mapping they gave us is correct, which it isn't
            -- print $ sum . map (toInt . foldl (\acc x1 -> acc ++ getNum x1) "" . snd) $ x
-- where
--     toInt str = read str :: Int
--     getNum lx
--         | length lx == 2 = "1"
--         | length lx == 4 = "4"
--         | length lx == 3 = "7"
--         | length lx == 7 = "8"
--     getNum x = error $ show x
-- | all (`elem` zero) lx  = "0"
-- | all (`elem` two) lx   = "2"
-- | all (`elem` three) lx = "3"
-- | all (`elem` five) lx  = "5"
-- | all (`elem` six) lx   = "6"
-- | all (`elem` nine) lx  = "9"

data Panel
    = Top       -- top row
    | Middle    -- middle row
    | Bottom    -- bottom row
    | One       -- top left row
    | Two       -- top right row
    | Three     -- bottom left row
    | Four      -- bottom right row
    deriving (Eq, Enum, Show, Read, Ord)

zero, one, two, three, four, five, six, seven, eight, nine :: Set Panel
zero    = Set.fromList [Top,         Bottom, One, Two, Three, Four]
one     = Set.fromList [                          Two,        Four] -- unique
two     = Set.fromList [Top, Middle, Bottom,      Two, Three      ] 
three   = Set.fromList [Top, Middle, Bottom,      Two,        Four]
four    = Set.fromList [     Middle,         One, Two,        Four] -- unique
five    = Set.fromList [Top, Middle, Bottom, One,             Four]
six     = Set.fromList [Top, Middle, Bottom, One,      Three, Four]
seven   = Set.fromList [Top,                      Two,        Four] -- unique
eight   = Set.fromList [Top, Middle, Bottom, One, Two, Three, Four] -- unique
nine    = Set.fromList [Top, Middle, Bottom, One, Two,        Four]

-- | assumes the list is sorted by the length of the strings and fulfills the segment boundaries!
-- decrypt :: [Set String] -> [Set Int]
-- decrypt xs = 
--     let one'  = xs !! 0 -- no head so I see more clearly whats going on
--         four' = xs !! 2 
--         seven'= xs !! 1
--         eight'= xs !! 5
--         top   = Set.difference seven' one'
        
--          in do


--    0        1         2                 3                            4                   5
-- [["db"], ["deb"], ["adfb"], ["aebdg","ecgba","gdfae"], ["dabegf","defgca","gecbfd"], ["efgbadc"]]


-- Parser

-- | left hand side
noteParser :: Parsec.ParsecT String u Identity [String]
noteParser = Parsec.sepBy (Parsec.many Parsec.letter) (Parsec.string " ") <* Parsec.spaces

-- | separator aka |
sep :: Parsec.ParsecT String u Identity ()
sep = Parsec.spaces >> Parsec.char '|' >> Parsec.spaces

-- | line parser
dayEightP' :: Parsec.ParsecT String u Identity ([String], [String])
dayEightP' = do
    left <- noteParser
    sep
    right <- noteParser
    return (left, right)
