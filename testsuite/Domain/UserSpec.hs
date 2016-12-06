module Domain.UserSpec (spec) where

import Test.Hspec
import Domain.User

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)