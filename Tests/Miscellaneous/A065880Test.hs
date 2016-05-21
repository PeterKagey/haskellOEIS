module Tests.Miscellaneous.A065880Test where
import Test.Hspec
import Miscellaneous.A065880 (a065880)
main :: IO ()

main = hspec $
  describe "A065880" $
    it "correctly computes the first 20 elements" $
      take 20 (map a065880 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,1,2,6,4,10,12,21,8,18,20,55,24,0,42,60,16,34,36,0]
