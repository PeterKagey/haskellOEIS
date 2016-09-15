module Tests.Palindromes.A249629Test where
import Test.Hspec
import Palindromes.A249629 (a249629)

main :: IO ()
main = hspec $
  describe "A249629" $
    it "correctly computes the first 10 elements" $
      take 10 (map a249629 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,0,4,28,124,532,2164,8788,35284,141628]
