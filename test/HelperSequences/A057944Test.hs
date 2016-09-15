module Tests.HelperSequences.A057944Test where
import Test.Hspec
import HelperSequences.A057944 (a057944)

main :: IO ()
main = hspec $
  describe "A057944" $
    it "correctly computes the first 5 elements" $
      take 5 (map a057944 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,1,1,3,3]
