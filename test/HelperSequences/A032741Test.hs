module Tests.HelperSequences.A032741Test where
import Test.Hspec
import HelperSequences.A032741 (a032741)

main :: IO ()
main = hspec $
  describe "A032741" $
    it "correctly computes the first 10 elements" $
      take 10 (map a032741 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,0,1,1,2,1,3,1,3,2]
