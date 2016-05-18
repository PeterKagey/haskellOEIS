module Tests.Subsets.A272036Test where
import Test.Hspec
import Subsets.A272036 (a272036)

main :: IO ()
main = hspec $
  describe "A272036" $
    it "correctly computes the first 3 elements" $
      take 3 (map a272036 [0..]) `shouldBe` expectedValue where
        expectedValue = [1,38,2090]
