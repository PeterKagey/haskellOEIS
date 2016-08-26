module Tests.Coins.A276163Test where
import Test.Hspec
import CoinsInARow.A276163 (a276163)

main :: IO ()
main = hspec $
  describe "A276163" $
    it "correctly computes the first 5 elements" $
      map a276163 [1..5] `shouldBe` expectedValue where
        expectedValue = [1, 1, 2, 4, 5]
