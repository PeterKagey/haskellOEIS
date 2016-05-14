module Tests.Palindromes.A249641Test where
import Test.Hspec
import Palindromes.A249641 (a249641)

main :: IO ()
main = hspec $
  describe "A249641" $
    it "correctly computes the first 10 elements" $
      take 10 (map a249641 [0..]) `shouldBe` expectedValue where
        expectedValue = [0,0,8,120,1016,8520,68552,551496,4415048,35344632]
