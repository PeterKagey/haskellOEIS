module Tests.EKG.A064413
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import EKG.A064413 (a064413)

main :: IO ()

main = hspec $ do
  describe "A064413" $ do
    it "correctly computes the first 20 elements" $ do
      take 20 (map a064413 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,2,4,6,3,9,12,8,10,5,15,18,14,7,21,24,16,20,22,11]
