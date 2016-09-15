module Tests.Miscellaneous.A000002Test where
import Test.Hspec
import Miscellaneous.A000002 (a000002)

main :: IO ()
main = hspec $
  describe "A000002" $
    it "correctly computes the first 20 elements" $
      take 20 (map a000002 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1]
