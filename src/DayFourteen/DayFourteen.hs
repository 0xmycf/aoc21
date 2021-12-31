module DayFourteen.DayFourteen
( mainDayFourteen
, testDayFourteen
) where

import           Common.Lib  (getLines, frequencyMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ((&&&))
import Data.List (unfoldr)

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayFourteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayFourteen.txt"

type Template = Map String Char

-- | parses the input depending on the FilePath given
input :: FilePath -> IO (String, Template)
input path = let inputs = filter (/=[]) <$> getLines path
             in do
                inp <- inputs
                let code = concat . take 1 $ inp
                    rest = Map.fromList . fmap toMap . drop 1 $ inp
                pure (code, rest)
                where
                toMap :: String -> (String, Char)
                toMap (a:b:' ':'-':'>':' ':c:_) = ([a,b], c)
                toMap x = error $ "Parse error at: " ++ x

mainDayFourteen :: IO ()
mainDayFourteen = putStrLn "Day Fourteen..." >> problemOne >> problemTwo >> putStrLn "Day Fourteen over.\n "

testDayFourteen :: IO ()
testDayFourteen = do
    putStrLn "Test Day Fourteen..."
    (str, tmp) <- input testPath
    print $ printPolymer str tmp
    putStrLn "Test Day Fourteen over.\n"


problemOne :: IO ()
problemOne = do
    (str, tmp) <- input inputPath
    print . factory str tmp $ 10

problemTwo :: IO ()
problemTwo = print "to be impl"

factory :: String -> Map String Char -> Int -> (Int, Int)
factory str tmp i = (minimum &&& maximum)
        . Map.elems . frequencyMap . last . take i . unfoldr (Just . printing tmp) $ str
        where
        printing tmp v = let a = printPolymer v tmp in (a,a)

-- | TODO find a better data structure to represent this and speed it up
printPolymer :: String -> Template -> String
printPolymer str tmp =
    let str' = zipWith charsToString str (drop 1 str)
    in assemble [] . fmap printing $ str'
    where
    printing :: String -> String
    printing s@(a:b:_)    = [a, tmp Map.! s, b]
    printing s            = error "Huh something went wrong on: " ++ s
    assemble :: String -> [String] -> String
    assemble acc []       = acc
    assemble acc [a]      = acc ++ a
    assemble acc (a:xs)   = assemble (acc ++ init a) xs

charsToString :: Char -> Char -> String
charsToString c c1 = [c,c1]
