module Tests.HelperSequences.A003415Test where
import Test.Hspec
import HelperSequences.A003415 (a003415)
main :: IO ()

main = hspec $
  describe "A003415" $
    it "correctly computes the first 20 elements" $
      take 20 (map a003415 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,0,1,1,4,1,5,1,12,6,7,1,16,1,9,8,32,1,21,1]
