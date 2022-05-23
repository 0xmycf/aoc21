import           Common.Lib (getNine)
import qualified Data.Set   as Set
import           Linear     (V2 (V2))
import           Test.Hspec (describe, hspec, it, shouldBe)

-- I should write more test cases
main :: IO ()
main = hspec do
  describe "testing getNine function" do
    it "V 0 0 -> V -1 -1 to V 1 1 in a 9x9 field (Comparison as Sets)" do
      Set.fromList (getNine (V2 0 0)) `shouldBe` Set.fromList [V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1, V2 0 (-1), V2 0 0, V2 0 1, V2 1 (-1), V2 1 0, V2 1 1]
