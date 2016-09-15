module Tests.HelperSequences.A000330Test where
import Test.Hspec
import HelperSequences.A000330 (a000330)

main :: IO ()
main = hspec $
  describe "A000330" $
    it "correctly computes the first 5 elements" $
      take 5 (map a000330 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,1,5,14,30]
