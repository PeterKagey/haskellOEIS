module Tests.Subsets.A272082Test where
import Test.Hspec
import Subsets.A272082 (a272082)

main :: IO ()
main = hspec $
  describe "A272082" $
    it "correctly computes the first 17 elements" $
      take 17 (map a272082 [0..]) `shouldBe` expectedValue where
        expectedValue = [1,6,3,2,6,3,2,1,12,6,4,2,12,6,4,2,1]
