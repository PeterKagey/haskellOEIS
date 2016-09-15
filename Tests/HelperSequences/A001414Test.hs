module Tests.HelperSequences.A001414Test where
import Test.Hspec
import HelperSequences.A001414 (a001414)
main :: IO ()

main = hspec $
  describe "A001414" $
    it "correctly computes the first 20 elements" $
      take 20 (map a001414 [1..]) `shouldBe` expectedValue where
        expectedValue = [0,2,3,4,5,5,7,6,6,7,11,7,13,9,8,8,17,8,19,9]
