module Tests.HelperSequences.A070229Test where
import Test.Hspec
import HelperSequences.A070229 (a070229)
main :: IO ()

main = hspec $
  describe "A070229" $
    it "correctly computes the first 20 elements" $
      take 20 (map a070229 [1..]) `shouldBe` expectedValue where
        expectedValue = [2,4,6,6,10,9,14,10,12,15,22,15,26,21,20,18,34,21,38,25]
