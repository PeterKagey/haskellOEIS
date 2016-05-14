module Tests.AlecSequences.A096216Test where
import Test.Hspec
import AlecSequences.A096216 (a096216)
main :: IO ()

main = hspec $ do
  describe "A096216" $ do
    it "correctly computes the first 20 elements" $ do
      take 20 (map a096216 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,2,2,4,2,6,2,7,3,10,3,12,4,9,6,16,3,18,7]
