module Tests.HelperSequences.A000523Test where
import Test.Hspec
import HelperSequences.A000523 (a000523)
main :: IO ()

main = hspec $
  describe "A000523" $
    it "correctly computes the first 20 elements" $
      take 20 (map a000523 [1..]) `shouldBe` expectedValue where
        expectedValue = [0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4]
