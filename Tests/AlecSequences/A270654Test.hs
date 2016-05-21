module Tests.AlecSequences.A270654Test where
import Test.Hspec
import AlecSequences.A270654 (a270654)
main :: IO ()

main = hspec $
  describe "A270654" $
    it "correctly computes the first 20 elements" $
      take 20 (map a270654 [1..]) `shouldBe` expectedValue where
        expectedValue = [0,1,1,5,1,14,1,4,14,17,1,42,9,27,23,42,44,47,37,70]
