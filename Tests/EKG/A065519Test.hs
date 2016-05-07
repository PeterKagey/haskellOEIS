import Test.Hspec
import EKG.A065519 (a065519)
main :: IO ()

main = hspec $
  describe "A065519" $
    it "correctly computes the first 20 elements" $
      take 20 (map a065519 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,2,4,6,3,9,12,8,10,5,15,18,14,7,21,24,16,20,22,11]
