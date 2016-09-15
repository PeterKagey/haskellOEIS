module Tests.HelperSequences.A005117Test where
import Test.Hspec
import HelperSequences.A005117 (a005117)
main :: IO ()

main = hspec $
  describe "A005117" $
    it "correctly computes the first 20 elements" $
      take 20 (map a005117 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,2,3,5,6,7,10,11,13,14,15,17,19,21,22,23,26,29,30,31]
