module Tests.Nonpalindromes.A252700Test where
import Test.Hspec
import Nonpalindromes.A252700 (a252700)

main :: IO ()
main = hspec $
  describe "A252700" $
    it "correctly computes the first 10 elements" $
      take 10 (map a252700 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,7,42,252,1722,11802,82362,574812,4021962,28141932]
