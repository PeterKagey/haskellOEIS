module Tests.EKG.A169847Test where
import Test.Hspec
import EKG.A169847 (a169847)
main :: IO ()

main = hspec $
  describe "A169847" $
    it "correctly computes the first 20 elements" $
      take 20 (map a169847 [1..]) `shouldBe` expectedValue where
        expectedValue = [8,2,4,6,3,9,12,10,5,15,18,14,7,21,24,16,20,22,11,33]
