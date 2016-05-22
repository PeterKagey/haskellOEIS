module Tests.Miscellaneous.A268038Test where
import Test.Hspec
import Miscellaneous.A268038 (a268038)
main :: IO ()

main = hspec $
  describe "A268038" $
    it "correctly computes the first 20 elements" $
      take 20 (map a268038 [1..]) `shouldBe` expectedValue where
        expectedValue = [0,0,-1,-1,-1,0,1,1,1,1,0,-1,-2,-2,-2,-2,-2,-1,0,1]
