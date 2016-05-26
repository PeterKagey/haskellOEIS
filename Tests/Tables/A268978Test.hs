module Tests.Tables.A268978Test where
import Test.Hspec
import Tables.A268978 (a268978)
main :: IO ()

main = hspec $
  describe "A268978" $
    it "correctly computes the first 20 elements" $
      take 20 (map a268978 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,3,0,6,1,0,10,1,2,0,15,4,3,2,0,21,6,3,2,4]
