module DayTwentyOne.DayTwentyOne
( mainDayTwentyOne
, testDayTwentyOne
) where

import           Common.Lib              (getLines)
import           Control.Arrow           ((&&&))
import           Data.Text.Internal.Read (digitToInt)

inputPath :: FilePath
inputPath = "./inputs/auto/input/2021/DayTwentyOne.txt"

testPath :: FilePath
testPath  = "./inputs/test/DayTwentyOne.txt"

outPath :: FilePath
outPath = "./inputs/test/parsedout/DayTwentyOne.txt"

data Pos a = Pos a a deriving (Eq, Show)

-- stuff i might or might not need
instance Functor Pos where
  fmap f (Pos a b) = Pos (f a) (f b)

instance Applicative Pos where
  pure a = Pos a a
  Pos f g <*> Pos a b = Pos (f a) (g b)

instance Semigroup a => Semigroup (Pos a) where
  Pos a b <> Pos c d = Pos (a <> c) (b <> d)

map1P, map2P :: (a -> a) -> Pos a -> Pos a
map1P f (Pos a b) = Pos (f a) b
map2P f (Pos a b) = Pos  a (f b)
mapBP :: (a -> b) -> (a -> b) -> Pos a -> Pos b
mapBP f g (Pos a b) = Pos (f a) (g b)
add, sub :: (Applicative f, Num b) => f b -> f b -> f b
add pos pos2 = (+) <$> pos <*> pos2
sub pos pos2 = (-) <$> pos <*> pos2
mapWhen :: When -> (a -> a) -> Pos a -> Pos a
mapWhen One = map1P
mapWhen Two = map2P

type Input = Pos Int

data Game = Game
  { gplayer1 :: Player
  , gplayer2 :: Player
  , gthrown  :: Int
  }

-- | parses the input depending on the FilePath given
input :: FilePath -> IO Input
input path = let inputs = toInput . fmap last <$> getLines path
             in inputs
            where toInput [o,t] = Pos (digitToInt o) (digitToInt t)
                  toInput _     = error "Something went terribly wrong"

mainDayTwentyOne :: IO ()
mainDayTwentyOne = putStrLn "Day TwentyOne..." >> input inputPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day TwentyOne over.\n "

testDayTwentyOne :: IO ()
testDayTwentyOne = putStrLn "Day TwentyOne..." >> input testPath >>= print . (problemOne &&& problemTwo) >> putStrLn "Day TwentyOne over.\n "

problemOne :: Input -> String
problemOne poz =
  let player1 = Player One (0 :: Int)
      player2 = Player Two (0 :: Int)
      game    = Game player1 player2 0
   in show $ play game poz prepedDice
  where play :: Game -> Input -> [Int] -> Int
        play game@(Game p1 p2 thrown) pos (x:xs)
          | any (>=1000) (scores game) =  lowerscore game * thrown
          | even x = let (newp1, newpos) = moveOnTrack p1 pos x in play (Game newp1 p2 (thrown + 3)) newpos xs
          | otherwise = let (newp2, newpos) = moveOnTrack p2 pos x in play (Game p1 newp2 (thrown + 3)) newpos xs
        play _ _ _ = error "foo"

scores :: Game -> [Int]
scores (Game p b _) = fmap gscore [p, b]

lowerscore :: Game -> Int
lowerscore = minimum . scores

-- infinite cyclic list 1..100
deterDice :: [Int]
deterDice = cycle [1..100]

prepedDice :: [Int]
prepedDice = fmap sum . groupX 3 $ deterDice

groupX :: Int -> [a] -> [[a]]
groupX _ [] = []
groupX n xs = let (ys, zs) = splitAt n xs
              in ys : groupX n zs

data When = One | Two deriving (Show, Eq)
data Playerr a = Player
  { gwhen  :: When
  , gscore :: a
  } deriving (Eq, Show)

type Player = Playerr Int


mapSPlayer :: (t -> a) -> Playerr t -> Playerr a
mapSPlayer f (Player when score) = Player when (f score)

instance Functor Playerr where
  fmap = mapSPlayer

{-
  Moving on the track is easily achievable through modulos
  here we have a track [1..10], so we would do mod 10 + 1, but the board is represented as [0..9]
  this maps as follows
  lets go one way around in steps of 1
  Starting at 1 (is 0)
  0 mod 10 + 1  == 1
  1 mod 10 + 1  == 2
  ...
  8 mod 10 + 1  == 9
  9 mod 10 + 1  == 10
  10 mod 10 + 1 == 1 (since 10 is actually 11 and therefore 1, we end up correctly)
  11 mod 10 + 1 == 2 (since 11 is actually 12 and therefore 2, we end up correctly)

  The function takes in a player, a position and the amount of steps and returns a tuple of player and updated Position
-}
moveOnTrack :: Player -> Pos Int -> Int -> (Player, Pos Int)
moveOnTrack p@(Player when _) pos steps = (fmap (+newPos) p, mapWhen when (const newPos) pos)
  where newPos = ((steps + playerPos - 1) `mod` 10) + 1
        playerPos = whenToPos when pos

-- lazy testing: why use hspec if you can do it like this lmao
testMove :: IO ()
testMove = do
  print $ (++) "1" $ show $ moveOnTrack (Player One 0) (Pos 1 1) (1)
  print $ moveOnTrack (Player One 0) (Pos 1 1) (2)
  print $ moveOnTrack (Player One 0) (Pos 1 1) (3)
  print $ moveOnTrack (Player One 0) (Pos 1 1) (4)
  print $ (++) "5" $ show $ moveOnTrack (Player One 0) (Pos 1 1) (5)
  print $ moveOnTrack (Player One 0) (Pos 1 1) (6)
  print $ moveOnTrack (Player One 0) (Pos 1 1) (7)
  print $ moveOnTrack (Player One 0) (Pos 1 1) (8)
  print $ moveOnTrack (Player One 0) (Pos 1 1) (9)
  print $ (++) "10" $ show $ moveOnTrack (Player One 4) (Pos 1 1) (10)
  print $ moveOnTrack (Player One 0) (Pos 1 1) (11)
  print $ moveOnTrack (Player One 0) (Pos 1 1) (12)

whenToPos :: When -> Input -> Int
whenToPos One (Pos a _) = a
whenToPos Two (Pos _ b) = b

problemTwo :: Input -> String
problemTwo = const "to be impl"
