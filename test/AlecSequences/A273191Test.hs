module Tests.AlecSequences.A273191Test where
import Test.Hspec
import AlecSequences.A273191 (a273191)
main :: IO ()

main = hspec $
  describe "A273191" $
    it "correctly computes the first 20 elements" $
      take 20 (map a273191 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,1,6,1,3,4,2,6,1,7,4,4,9,1,10,4,8,9,3]
