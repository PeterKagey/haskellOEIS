module Tests.Subsets.A272081Test where
import Test.Hspec
import Subsets.A272081 (a272081)

main :: IO ()
main = hspec $
  describe "A272081" $
    it "correctly computes the first 12 elements" $
      take 12 (map a272081 [0..]) `shouldBe` expectedValue where
        expectedValue = [1,2,3,4,5,6,6,3,6,3,2,7]
