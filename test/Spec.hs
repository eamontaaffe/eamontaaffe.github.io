import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Site
import Text.Regex

main :: IO ()
main = hspec $ do
  describe "getCategoryFromPath" $ do
    it "should work..." $ do
      (getCategoryFromPath "/events/001.abc.book.md") `shouldBe` (Just "book")
