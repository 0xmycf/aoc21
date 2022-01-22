{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DayEighteen.DayEighteen
( mainDayEighteen
, testDayEighteen
) where

import           Common.Lib            (getLines, parse, updiv)
import           Control.Applicative   (Alternative ((<|>)))
import           Control.Arrow         ((&&&))
import           Data.Char             (digitToInt)
import           Data.Functor.Identity (Identity)
import           Data.Maybe            (isNothing)
import           Text.Parsec           (ParsecT)
import qualified Text.Parsec           as P

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayEighteen.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayEighteen.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DayEighteen.txt"

type Input = [SnailNum Int]

data SnailNum a = SNum                          -- ^ SnailNum composed with two other SnailNums
                { lef :: SnailNum a
                , rig :: SnailNum a
                }
              | JNum                          -- ^ A Snail Number that is just a normal number, with a function for finding the depth
                { num   :: a
                }
    deriving (Eq)

instance Show a => Show (SnailNum a) where
    show (SNum l r) = "{|" ++ show l ++  "," ++ show r ++  "|}"
    show (JNum v)   = show v

instance (Integral a, Ord a, Num a, Eq a) => Semigroup (SnailNum a) where
  JNum i <> JNum j = JNum (i + j)
  snum   <> snum1  = reduceS' $ SNum snum snum1

data Crumb  a = LC (SnailNum a) | RC (SnailNum a) deriving (Show, Eq)
type Crumbs a = [Crumb a]

type Zipper a = (Crumbs a, SnailNum a)

isLeft :: Crumb a -> Bool
isLeft (LC _) = True
isLeft _      = False

left :: Zipper a -> Zipper a
left (as, SNum l r) = (LC r:as, l)

right :: Zipper a -> Zipper a
right (as, SNum l r) = (RC l:as, r)

up :: Zipper a -> Zipper a
up (LC snum : as, snum1) = (as, SNum snum1 snum)
up (RC snum : as, snum1) = (as, SNum snum snum1)

goToTop :: Zipper a -> Zipper a
goToTop zips@([], _) = zips
goToTop zipper       = goToTop (up zipper)

goTo :: Zipper a -> [Crumb a] -> Zipper a
goTo = foldr (\v acc -> if isLeft v then left acc else right acc)

fgoTo :: [Crumb a] -> Zipper a -> Zipper a
fgoTo = flip goTo

addVeryLeft :: Num a => a -> Zipper a -> Zipper a
addVeryLeft j (as, SNum (JNum i) jnum) = (as, SNum (JNum (i + j)) jnum)
addVeryLeft j zipper                   = addVeryLeft j (left zipper)

addVeryRight :: Num a => a -> Zipper a -> Zipper a
addVeryRight j (as, SNum jnum (JNum i)) = (as, SNum jnum (JNum (i + j)))
addVeryRight j zipper                   = addVeryRight j (right zipper)

reduceS' :: (Integral a, Ord a, Num a, Eq a) => SnailNum a -> SnailNum a
reduceS' snum = maybe snum reduceS' $ explode snum <|> zplit snum

-- butchered...
-- there are better and shorter ways to do this...
-- one I got more training with trees i'll come back
explode :: (Num a, Eq a) => SnailNum a -> Maybe (SnailNum a)
explode = fmap snd . go 0 . ([],)
    where
    go :: (Num a, Eq a) => Int -> Zipper a -> Maybe (Zipper a)
    go 4 (as, SNum l r) = toZero . goToTop <$> doBoth
        where
        doBoth = let zipper = addLeft Nothing l (as, SNum l r) -- up call removed
                 in if isNothing zipper
                    then addRight Nothing r (as, SNum l r) -- up call removed
                    else let newzip = fgoTo as . goToTop <$> zipper -- up call removed
                         in case addRight Nothing r =<< newzip of
                            Nothing -> zipper
                            zipperr -> zipperr

        toZero = goToTop . lam . fgoTo as
            where
                lam (las, SNum _ _) = (las, JNum 0)
                lam _               = error "That shouldn't happen"

    go _ (_:_, JNum _)      = Nothing
    go i zipper             = go (i+1) (left zipper) <|> go (i+1) (right zipper)

    addLeft, addRight :: (Num a, Eq a) => Maybe(Crumb a) -> SnailNum a -> Zipper a -> Maybe (Zipper a)
    addRight (Just c) (JNum j) z@([], _)        = if not . isLeft $ c
                                                  then Nothing
                                                  else case z of
                                                    (_, SNum _ (JNum _))   -> Just . addVeryRight j $ z
                                                    (_, SNum _ (SNum _ _)) -> Just . addVeryLeft  j $ right z
    addRight _ (JNum j) z@(LC _ : _, _) = case up z of
                                                    zz@(_, SNum _ (JNum _))   -> Just . addVeryRight j $ zz
                                                    zz@(_, SNum _ (SNum _ _)) -> Just . addVeryLeft  j $ right zz
    addRight _ j snum1@(s:_,_)                  = addRight (Just s) j (up snum1)
    addLeft (Just c) (JNum j) z@([], _)         = if isLeft c
                                                  then Nothing
                                                  else case z of
                                                    (_, SNum (JNum _) _)   -> Just . addVeryLeft  j $ z
                                                    (_, SNum (SNum _ _) _) -> Just . addVeryRight j $ left z
    addLeft _ (JNum j) zz@(RC _ : _, _) = case up zz of
                                                    z@(_, SNum (JNum _) _)   -> Just . addVeryLeft  j $ z
                                                    z@(_, SNum (SNum _ _) _) -> Just . addVeryRight j $ left z
    addLeft _ j snum1@(s:_,_)                   = addLeft (Just s) j (up snum1)

zplit :: (Integral a, Num a, Ord a) => SnailNum a -> Maybe (SnailNum a)
zplit = fmap snd . go . ([],)
    where
        go (as, JNum j)
            | j >= 10 = Just . goToTop $ (as, SNum (JNum (j `div` 2)) (JNum (j `updiv` 2)))
            | otherwise = Nothing
        go zipper = go (left zipper) <|> go (right zipper)


magnitude :: (Num a) => SnailNum a -> a
magnitude (SNum l r) = 3 * magnitude l + 2 * magnitude r
magnitude (JNum p)   = p

{-
  Parsing
-}

-- | Parsers a full SnailNum
snumParser :: ParsecT String u Identity (SnailNum Int)
snumParser = do
    _  <- P.char '['
    i1 <- jnumParser P.<|> snumParser
    _  <- P.char ','
    i2 <- jnumParser P.<|> snumParser
    _  <- P.char ']'
    pure $ SNum i1 i2

-- | The most shallow SnailNum instance
jnumParser :: ParsecT String u Identity (SnailNum Int)
jnumParser = JNum . digitToInt <$> P.digit

-- | parses the input depending on the FilePath given
input :: FilePath -> IO Input
input path = fmap (\case
                Left  pe -> error $ show pe
                Right pa -> pa
              . parse snumParser) <$> getLines path

mainDayEighteen :: IO ()
mainDayEighteen = putStrLn "Day Eighteen..." >> input inputPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day Eighteen over.\n "

testDayEighteen :: IO ()
testDayEighteen = putStrLn "Day Eighteen..." >> input testPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day Eighteen over.\n "

problemOne :: Input -> Int
problemOne inp = do
    magnitude . foldl1 (<>) $ inp

-- Debug function that yields all intermediate states of the computation
writeMe :: IO ()
writeMe = do
    inp <- input testPath
    let foo = scanl1 (<>) inp
    writeFile outPath (show foo)

problemTwo :: Input -> String
problemTwo = const "to be impl"

