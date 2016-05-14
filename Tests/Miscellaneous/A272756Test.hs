module Tests.Miscellaneous.A272756Test where
import Test.Hspec
import Miscellaneous.A272756 (a272756)

main :: IO ()
main = hspec $
  describe "A272756" $
    it "correctly computes the first 20 elements" $
      take 20 (map a272756 [1..]) `shouldBe` expectedValue where
        expectedValue = [3,5,5,6,6,7,7,7,7,8,8,8,8,8,8,9,9,9,9,9]
