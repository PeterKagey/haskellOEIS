module Tests.HelperSequences.A000194Test where
import Test.Hspec
import HelperSequences.A000194 (a000194)
main :: IO ()

main = hspec $
  describe "A000194" $
    it "correctly computes the first 20 elements" $
      take 20 (map a000194 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,4,4]
