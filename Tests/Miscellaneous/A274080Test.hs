module Tests.Miscellaneous.A274080Test where
import Test.Hspec
import Miscellaneous.A274080 (a274080)

main :: IO ()
main = hspec $
  describe "A274080" $
    it "correctly computes the first 20 elements" $
      take 20 (map a274080 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,2,1,2,1,2,3,4,1,3,4,5,1,2,4,2,3,4,5]
