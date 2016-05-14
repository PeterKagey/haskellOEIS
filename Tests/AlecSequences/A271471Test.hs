module Tests.AlecSequences.A271471Test where
import Test.Hspec
import AlecSequences.A271471 (a271471)
main :: IO ()

main = hspec $
  describe "A271471" $
    it "correctly computes the first 5 elements" $
      take 5 (map a271471 [1..]) `shouldBe` expectedValue where
        expectedValue = [5,17,28,37,82]
