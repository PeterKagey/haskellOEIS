module Tests.Miscellaneous.A275673Test where
import Test.Hspec
import Miscellaneous.A275673 (a275673)
main :: IO ()

main = hspec $
  describe "A275673" $
    it "correctly computes the first 20 elements" $
      take 20 (map a275673 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,2,3,4,5,6,7,9,11,13,15,17,19,22,25,28,31,34,37,41]
