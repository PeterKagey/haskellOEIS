module Tests.Miscellaneous.A259439Test where
import Test.Hspec
import Miscellaneous.A259439 (a259439)
main :: IO ()

main = hspec $
  describe "A259439" $
    it "correctly computes the first 20 elements" $
      take 20 (map a259439 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,1,1,1,2,1,2,3,5,3,6,3,4,3,4,3,6,3,5]
