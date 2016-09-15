module Tests.Miscellaneous.A261865Test where
import Test.Hspec
import Miscellaneous.A261865 (a261865)
main :: IO ()

main = hspec $
  describe "A261865" $
    it "correctly computes the first 20 elements" $
      take 20 (map a261865 [1..]) `shouldBe` expectedValue where
        expectedValue = [2,2,3,2,2,3,2,2,2,3,2,2,3,2,2,2,3,2,2,3]
