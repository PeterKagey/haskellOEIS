module Tests.Miscellaneous.A143483Test where
import Test.Hspec
import Miscellaneous.A143483 (a143483)
main :: IO ()

main = hspec $
  describe "A143483" $
    it "correctly computes the first 20 elements" $
      take 20 (map a143483 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,2,2,4,4,6,8,18,20,20,24,24,24,24,32,32,36,36,40]
