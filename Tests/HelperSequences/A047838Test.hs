import Test.Hspec
import HelperSequences.A047838 (a047838)

main :: IO ()
main = hspec $
  describe "A047838" $
    it "correctly computes the first 10 elements" $
      take 10 (map a047838 [2..]) `shouldBe` expectedValue where
        expectedValue = [1,3,7,11,17,23,31,39,49,59]
