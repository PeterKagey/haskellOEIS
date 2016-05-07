import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import AlecSequences.A269423 (a269423)
main :: IO ()

main = hspec $ do
  describe "A269423" $ do
    it "correctly computes the first 20 elements" $ do
      take 20 (map a269423 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,3,1,7,4,8,8,10,16,3,9,7,12,13,25,12,4,12,14]
