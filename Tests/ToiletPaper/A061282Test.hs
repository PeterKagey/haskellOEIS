module Tests.ToiletPaper.A061282Test where
import Test.Hspec
import ToiletPaper.A061282 (a061282)
main :: IO ()

main = hspec $
  describe "A061282" $
    it "correctly computes the first 20 elements" $
      take 20 (map a061282 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,1,2,2,3,4,3,4,5,3,4,5,4,5,6,5,6,7,4,5]
