module Tests.Miscellaneous.A065413Test where
import Test.Hspec
import Miscellaneous.A065413 (a065413)

main :: IO ()
main = hspec $
  describe "A065413" $
    it "correctly computes the first 20 elements" $
      take 20 (map a065413 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1]
