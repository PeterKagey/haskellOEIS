module Tests.HelperSequences.A070939Test where
import Test.Hspec
import HelperSequences.A070939 (a070939)

main :: IO ()
main = hspec $
  describe "A070939" $
    it "correctly computes the first 20 elements" $
      take 20 (map a070939 [0..]) `shouldBe` expectedValue where
        expectedValue = [1,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5]
