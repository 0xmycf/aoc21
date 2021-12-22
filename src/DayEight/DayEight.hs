module DayEight.DayEight
( mainDayEight
, testDayEight
) where

import           Common.Lib            (getLines, mapIdx, maskChar, parse)
import           Data.Foldable         (Foldable (foldl'), toList, traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Sequence         (sortOn)

import           Text.Parsec           ((<|>))
import qualified Text.Parsec           as Parsec

import           Data.Map              (Map)
import qualified Data.Map              as Map

import           Data.List             (groupBy)
import qualified Data.Sequence         as Seq

import           Data.Set              (Set)
import qualified Data.Set              as Set

inputPath :: String
inputPath = "./inputs/DayEight.txt"

testPath :: String
testPath  = "./inputs/test/DayEight.txt"

mainDayEight :: IO ()
mainDayEight = putStrLn "Day Eight..." >> problemOne >> problemTwo >> putStrLn "Day Eight over.\n "

testDayEight :: IO ()
testDayEight = do
    putStrLn "Test Day Eight..."
    pInput <- mapM (parse dayEightP') <$> getLines testPath
    case pInput of
      Left  pe -> print pe
      Right x  -> do
            let first  = fmap (arrange . filter (/=[]) . fmap (filter (/="")) . groupBy (\a b -> length a == length b) . toList . sortOn length . Seq.fromList . fst) x
                second = fmap snd x
            print first
            print $ fmap (fmap (fmap (foldr (flip maskChar) 0))) first
            print $ fmap decrypt first
        where
            -- another horrible function
            arrange [a,b,c,s,l,e] = [a,b,c,e,l,s]
            arrange             _ = error "this should never occur"
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
      Right x  -> do -- x :: [([String], [String])] -- . fmap (foldr maskChar 0) --
            let first  = fmap (arrange . filter (/=[]) . fmap (filter (/="")) . groupBy (\a b -> length a == length b) . toList . sortOn length . Seq.fromList . fst) x
                second = fmap (fmap (foldr (flip maskChar) 0) . snd) x
            let decrypted = fmap decrypt first
            print $ sum . fmap toInt . mapIdx (\intList idx -> -- intList :: [Integer]
                                let cm = decrypted !! idx in
                                    foldr (\int value ->
                                        Map.foldrWithKey (\key mpval acc ->
                                            if int == key
                                            then acc ++ show mpval
                                            else acc
                                        ) "" cm ++ value
                                    ) "" intList
                                ) $ second
        where
            -- | another horrible function
            arrange [a,b,c,s,l,e] = [a,b,c,e,l,s]
            arrange             _ = error "this should never occur"
            -- | I still don't know how to tell the compiler what read I want to use
            toInt :: String -> Int
            toInt = read

-- | None of the following 2 definitions is used, but it looks cool, so it stays
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

{-
    Im sorry future me (if you try to fix this again)!
-}
-- | How to write imperative style code in haskell 101
-- | for this function you need to pray and hope everything goes according to plan (it somehow did)
decrypt :: [[String]] -> Map Integer Int
decrypt xs = snd . foldl' decodedToMap (Map.empty, Map.empty) $ xs
    where -- idk this is horrible, but idk how to make it better atm...
        decodedToMap :: (Map Integer String, Map Integer Int) -> [String] -> (Map Integer String, Map Integer Int)
        decodedToMap (inv,mp) [a]
            | length a == 2 = (Map.insert 1 a inv, Map.insert (foldr (flip maskChar) 0 a) 1 mp)
            | length a == 3 = (Map.insert 7 a inv, Map.insert (foldr (flip maskChar) 0 a) 7 mp)
            | length a == 4 = (Map.insert 4 a inv, Map.insert (foldr (flip maskChar) 0 a) 4 mp)
            | length a == 7 = (Map.insert 8 a inv, Map.insert (foldr (flip maskChar) 0 a) 8 mp)
        decodedToMap (inv, mp) [a, b, c]
            | length a == 5 = -- shorts
                let sh  = Set.fromList [a, b, c]
                    one = Set.fromList ((xs !! 0) !! 0) -- => one MUST be on the left hand side, otherwise it wont work!
                    nine= Set.fromList (inv Map.! 9)
                    shorts acc a' =
                        let (inv,mp) = acc
                            aSet     = Set.fromList [a'] in
                        if all (`elem` Set.fromList a') one
                        then
                            foldr (\v _ ->
                                if Set.difference (Set.fromList v) nine == Set.empty
                                then do -- use bimap here??
                                    let t = (Map.insert 3 a' inv, Map.insert (foldr (flip maskChar) 0 a') 3 mp)
                                    let b = (Map.insert 5 v (fst t), Map.insert (foldr (flip maskChar) 0 v) 5 (snd t))
                                    let y = head . toList . Set.difference sh $ (Set.fromList [a', v])
                                    (Map.insert 2 y (fst b), Map.insert (foldr (flip maskChar) 0 y) 2 (snd b))
                                else do
                                    let t = (Map.insert 3 a' inv, Map.insert (foldr (flip maskChar) 0 a') 3 mp)
                                    let b = (Map.insert 2 v (fst t), Map.insert (foldr (flip maskChar) 0 v) 2 (snd t))
                                    let y = head . toList . Set.difference sh $ (Set.fromList [a', v])
                                    (Map.insert 5 y (fst b), Map.insert (foldr (flip maskChar) 0 y) 5 (snd b))
                                ) (Map.empty, Map.empty) (Set.difference sh aSet)
                        else acc
                    in let (resl, resr) = foldl shorts (Map.empty, Map.empty) [a, b, c]
                    in (inv `Map.union` resl, mp `Map.union` resr)
                | length a == 6 = -- longs
                    let ls = Set.fromList [a, b, c]
                        four = inv Map.! 4
                        one  = inv Map.! 1
                        longs acc a' =
                            let (inv,mp) = acc in
                            if not . all (`elem` a') $ one
                            then let diff = Set.difference ls (Set.fromList [a'])
                                in foldl (\acc v ->
                                    if all (`elem` Set.fromList four) (Set.difference (Set.fromList a') (Set.fromList v))
                                    then do
                                        let t = (Map.insert 6 a' inv, Map.insert (foldr (flip maskChar) 0 a') 6 mp)
                                        let b = (Map.insert 0 v (fst t), Map.insert (foldr (flip maskChar) 0 v) 0 (snd t))
                                        let y = head . toList . Set.difference ls $ (Set.fromList [a', v])
                                        (Map.insert 9 y (fst b), Map.insert (foldr (flip maskChar) 0 y) 9 (snd b))
                                    else do
                                    let t = (Map.insert 6 a' inv, Map.insert (foldr (flip maskChar) 0 a') 6 mp)
                                    let b = (Map.insert 9 v (fst t), Map.insert (foldr (flip maskChar) 0 v) 9 (snd t))
                                    let y = head . toList . Set.difference ls $ (Set.fromList [a', v])
                                    (Map.insert 0 y (fst b), Map.insert (foldr (flip maskChar) 0 y) 0 (snd b))
                                ) (Map.empty, Map.empty) diff
                            else acc
                    in let (resl, resr) = foldl longs (Map.empty, Map.empty) [a, b, c]
                    in (inv `Map.union` resl, mp `Map.union` resr)
        decodedToMap (inv, mp) xs' = error "Should never occur"

{-
My reasoning in ghci


    not . all (`elem` (longs !! 1)) $ "ab"
True
    all (`elem` (Set.fromList "eafb"))  (Set.difference (Set.fromList (longs !! 1)) (Set.fromList (longs !! 0)))
False
    all (`elem` (Set.fromList "eafb"))  (Set.difference (Set.fromList (longs !! 1)) (Set.fromList (longs !! 2)))
True
    longs !! 0
"cefabd" --> 9
    longs !! 1
"cdfgeb" --> 6
    longs !! 2
"cagedb" --> 0
    (Set.difference (Set.fromList (longs !! 1)) (Set.fromList (longs !! 2)))
fromList "f"
    longs
["cefabd","cdfgeb","cagedb"]

for shorts:

if all of ONE are in x then x = 3
if y without 9 is empty
    then y is 5; x = 3, v = 2

    else y is 2; x = 3, v = 5
-}
