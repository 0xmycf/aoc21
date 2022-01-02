module Common.Graph
( adjFromTuple
, adjFromNeighbs
) where

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (isJust)
import           Data.Set   (Set)
import qualified Data.Set   as Set
import           Data.Tuple (swap)
import Common.Lib (getNeighbors, Point)

type AdjacencyMap a = Map a (Set a)

adjFromTuple :: Ord a => [(a, a)] -> AdjacencyMap a
adjFromTuple xs = let xs' = xs ++ fmap swap xs in Map.fromListWith Set.union (fmap (fmap Set.singleton) xs')

adjFromNeighbs :: (Point -> Bool) -> Map Point v -> AdjacencyMap Point
adjFromNeighbs f mp = Map.fromList $ Map.foldlWithKey pairing [] mp
    where
        pairing :: [(Point, Set Point)] -> Point -> v -> [(Point, Set Point)]
        pairing acc key _ = let nbs = filter (\v -> let vm = Map.lookup v mp in isJust vm && f v) $ getNeighbors key
                            in (key, Set.fromList nbs) : acc

