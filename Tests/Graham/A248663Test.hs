module Tests.Graham.A248663Test where
import Test.Hspec
import Graham.A248663 (a248663)
main :: IO ()

main = hspec $
  describe "A248663" $
    it "correctly computes the first 20 elements" $
      take 20 (map a248663 [1..]) `shouldBe` expectedValue where
        expectedValue = [0,1,2,0,4,3,8,1,0,5,16,2,32,9,6,0,64,1,128,4]
