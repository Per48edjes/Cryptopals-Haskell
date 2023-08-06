import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "hammingDistance" $ do
        it "calculate correct Hamming distance" $ do
            hammingDistance "this is a test" "wokka wokka!!!" `shouldBe` 37
