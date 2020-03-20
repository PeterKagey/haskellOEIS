module HypercubePolyforms.A333362Spec (main, spec) where
import Test.Hspec
import HypercubePolyforms.A333362 (a333362)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A333362" $
  it "correctly computes the first 4 elements" $
    map a333362 [1..4] `shouldBe` expectedValue where
      expectedValue = [1,1,3,7]
