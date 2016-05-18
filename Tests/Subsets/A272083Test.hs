module Tests.Subsets.A272083Test where
import Test.Hspec
import Subsets.A272083 (a272083)

main :: IO ()
main = hspec $
  describe "A272083" $
    it "correctly computes the first 8 elements" $
      take 8 (map a272083 [0..]) `shouldBe` expectedValue where
        expectedValue = [1,6,3,2,12,6,4,2]
