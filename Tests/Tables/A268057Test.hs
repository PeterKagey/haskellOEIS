module Tests.Tables.A268057Test where
import Test.Hspec
import Tables.A268057 (a268057)
main :: IO ()

main = hspec $
  describe "A268057" $
    it "correctly computes the first 20 elements" $
      take 20 (map a268057 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,1,1,2,1,1,1,2,1,1,2,3,2,1,1,1,1,2,2]
