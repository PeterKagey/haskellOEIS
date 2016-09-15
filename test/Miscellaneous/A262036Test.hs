module Tests.Miscellaneous.A262036Test where
import Test.Hspec
import Miscellaneous.A262036 (a262036)
main :: IO ()

main = hspec $
  describe "A262036" $
    it "correctly computes the first 20 elements" $
      take 8 (map a262036 [2..]) `shouldBe` expectedValue where
        expectedValue = [1,3,40,61,23,170,99,54]
