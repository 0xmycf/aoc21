{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
import           Criterion               (bench, bgroup, nfIO)
import qualified Criterion.Main          as CMain
import qualified DayFifteen.DayFifteen   as DayFifteen


main :: IO ()
main = do
    CMain.defaultMain [
      bgroup "Day 15" [ bench "1" $ nfIO DayFifteen.problemOne,    -- bench "example" $ whnf exampleBench 100000000
                       bench "2" $ nfIO DayFifteen.problemTwo
                      ]
      ]
