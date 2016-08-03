module HappStackSpec (spec) where

--TODO: wie importiert man Main?
import Main
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "foo" $ do
        it "returns the unit value" $ do
            foo `shouldBe` ()

        prop "equals the unit value" $
            \ x -> foo == x