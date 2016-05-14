module Tests.HelperSequences.A025581Test where
import Test.Hspec
import HelperSequences.A025581 (a025581)

main :: IO ()
main = hspec $
  describe "A025581" $
    it "correctly computes the first 20 elements" $
      take 20 (map a025581 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,1,0,2,1,0,3,2,1,0,4,3,2,1,0,5,4,3,2,1]
