-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec
-- This adds quickCheck tests
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

main :: IO ()
main = do
    test <- testSpec "cairo-snake" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "is trivially true" $ do
        True `shouldBe` True
    it "works on greater" $ do
      (2 > 1) `shouldBe` True
