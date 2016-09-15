module Tests.Tables.A004489Test where
import Test.Hspec
import Tables.A004489 (a004489)
main :: IO ()

main = hspec $
  describe "A004489" $
    it "correctly computes the first 20 elements" $
      take 20 (map a004489 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,1,1,2,2,2,3,0,0,3,4,4,1,4,4,5,5,5,5,5]
