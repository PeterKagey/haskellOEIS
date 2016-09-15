module Tests.Miscellaneous.A053797Test where
import Test.Hspec
import Miscellaneous.A053797 (a053797)
main :: IO ()

main = hspec $
  describe "A053797" $
    it "correctly computes the first 20 elements" $
      take 20 (map a053797 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,2,1,1,1,1,2,2,1,1,1,2,3,1,1,1,1,2,1,1]
