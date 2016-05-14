module Tests.AlecSequences.A269347Test where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import AlecSequences.A269347 (a269347)
main :: IO ()

main = hspec $ do
  describe "A269347" $ do
    it "correctly computes the first 20 elements" $ do
      take 20 (map a269347 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,3,3,3,15,3,3,30,3,3,51,3,3,84,3,3,111,3,3]
