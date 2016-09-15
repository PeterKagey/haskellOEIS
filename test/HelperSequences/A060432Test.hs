module Tests.HelperSequences.A060432Test where
import Test.Hspec
import HelperSequences.A060432 (a060432)

main :: IO ()
main = hspec $
  describe "A060432" $
    it "correctly computes the first 5 elements" $
      take 5 (map a060432 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,3,5,8,11]
