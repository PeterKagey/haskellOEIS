module HelperSequences.A060432Spec (main, spec) where
import Test.Hspec
import HelperSequences.A060432 (a060432)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A060432" $
    it "correctly computes the first 5 elements" $
      take 5 (map a060432 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,3,5,8,11]
