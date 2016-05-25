module Tests.AlecSequences.A273190Test where
import Test.Hspec
import AlecSequences.A273190 (a273190)
main :: IO ()

main = hspec $
  describe "A273190" $
    it "correctly computes the first 20 elements" $
      take 20 (map a273190 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,1,0,1,1,1,1,1,1,2,1,1,1,2,2,2,2,1,1,2]
