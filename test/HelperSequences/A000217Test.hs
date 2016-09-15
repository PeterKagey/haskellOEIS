module Tests.HelperSequences.A000217Test where
import Test.Hspec
import HelperSequences.A000217 (a000217)

main :: IO ()
main = hspec $
  describe "A000217" $
    it "correctly computes the first 20 elements" $
      take 5 (map a000217 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,1,3,6,10]
