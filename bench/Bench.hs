
import Criterion (bench, whnf)
import qualified Criterion.Main      as CMain


main :: IO ()
main = do
    CMain.defaultMain 
        [ bench "example" $ whnf exampleBench 100000000
        ] 


-- | creates a list of length Int containing Int
-- | A simpler way would be take i [i..]
exampleBench :: Int -> [Int]
exampleBench i = listing i []
    where listing 0 acc = acc
          listing i acc  = listing (i-1) (1 : acc) 

{-
    The above example produced:

    benchmarking example
    time                 3.659 s    (2.595 s .. 5.992 s)
                         0.952 R²   (0.926 R² .. 1.000 R²)
    mean                 4.951 s    (4.284 s .. 5.489 s)
    std dev              670.1 ms   (323.7 ms .. 867.6 ms)
    variance introduced by outliers: 24% (moderately inflated)
-}