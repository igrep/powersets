import           Data.List       (sort)
import           Test.QuickCheck (mapSize, property)
import           Test.Syd        (describe, it, shouldBe, shouldReturn, sydTest)

import qualified IPPC
import           Lib


main :: IO ()
main = sydTest $
  describe "Various powerset functions" $ do
    it "should calculate the powerset of the given list" $ do
      let xs = [1, 3, 4] :: [Word]
          expected =
            [[], [1], [1, 3], [1, 3, 4], [1, 4], [3], [3, 4], [4]]
      sort (powersetRecursive xs) `shouldBe` expected
      sort (powersetFilterM xs) `shouldBe` expected
      sort (powersetFold xs) `shouldBe` expected

      IPPC.call "rec" xs `shouldReturn` expected
      IPPC.call "bintree" xs `shouldReturn` expected
      IPPC.call "reduce" xs `shouldReturn` expected

    it "should return the same powerset for the same arguments" $
      -- 5 was the best size for my machine to finish the test
      property . mapSize (`div` 5) $ \xs -> do
        let expected = sort (powersetRecursive (xs :: [Word]))
        sort (powersetFilterM xs) `shouldBe` expected
        sort (powersetFold xs) `shouldBe` expected

        IPPC.call "rec" xs `shouldReturn` expected
        IPPC.call "bintree" xs `shouldReturn` expected
        IPPC.call "reduce" xs `shouldReturn` expected
