module Tests.Miscellaneous.A272759Test where
import Test.Hspec
import Miscellaneous.A272759 (a272759)

main :: IO ()
main = hspec $
  describe "A272759" $
    it "correctly computes the first 30 elements" $
      take 30 (map a272759 [1..]) `shouldBe` expectedValue where
        expectedValue = [1,1,2,1,2,2,3,1,2,2,5,2,0,3,4,1,2,2,0,2,6,5,3,2,0,0,3,3,4,4]
