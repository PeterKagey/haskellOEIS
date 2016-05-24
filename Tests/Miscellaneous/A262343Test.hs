module Tests.Miscellaneous.A262343Test where
import Test.Hspec
import Miscellaneous.A262343 (a262343)
main :: IO ()

main = hspec $
  describe "A262343" $
    it "correctly computes the first 20 elements" $
      take 20 (map a262343 [3..]) `shouldBe` expectedValue where
        expectedValue = [1,3,9,2,15,9,7,12,27,5,33,18,13,21,45,8,51,27,19,30]
