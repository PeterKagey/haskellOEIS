import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import EKG.A065519 (a065519)
main :: IO ()

main = hspec $ do
  describe "A065519" $ do
    it "correctly computes the first 20 elements" $ do
      take 20 (map a065519 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,2,4,6,3,9,12,8,10,5,15,18,14,7,21,24,16,20,22,11]
