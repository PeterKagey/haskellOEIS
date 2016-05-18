module Tests.Subsets.A272035Test where
import Test.Hspec
import Subsets.A272035 (a272035)

main :: IO ()
main = hspec $
  describe "A272035" $
    it "correctly computes the first 20 elements" $
      take 6 (map a272035 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,1,38,39,2090,2091]
