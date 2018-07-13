module Data.Repository.Acid.UserSpec (spec) where

import Test.Hspec
import Data.Maybe                 ( isJust, fromJust, isNothing)
import Data.Acid                  ( AcidState, query, update )

import Data.Repository.Acid.DataBaseHelper   ( initDatabase )
import Data.Domain.User               as User

import qualified Data.Repository.Acid.User               as   UserAcid
import qualified Data.Repository.Acid.InterfaceAcid      as   InterfaceAcid

withDatabaseConnection :: (AcidState UserAcid.UserList -> IO ()) -> IO ()
withDatabaseConnection = initDatabase User{ User.name="Foo", userId=0, calendarEntries=[], belongingTasks=[]}

spec :: Spec
spec =
    around withDatabaseConnection $
        context "User" $
          describe "find" $
              it "by Username" $
                \c -> do
                  userState   <- query c $ UserAcid.FindByName "Foo"
                  userState `shouldSatisfy` isJust
                  fromJust userState `shouldBe` User{  User.name="Foo", userId=0, calendarEntries=[], belongingTasks=[] }
