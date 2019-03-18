module Miscellaneous.A321853Spec (main, spec) where
import Test.Hspec
import Miscellaneous.A321853 (a321853)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A321853" $
  it "correctly computes the first 6 elements" $
    map a321853 [1..6] `shouldBe` expectedValue where
      expectedValue = [0, 1, 10, 86, 756, 7092]
