module Data.Repository.Acid.UserAcidSpec (spec) where

import Test.Hspec
import Data.Maybe           ( isJust, fromJust, isNothing)
import Data.Acid            ( AcidState, query, update )

import Data.Repository.Acid.DataBaseHelper   ( initDatabase )
import Data.Repository.Acid.UserAcid as UserAcid
import Data.Domain.User as User
import qualified Data.Repository.Acid.InterfaceAcid      as   InterfaceAcid


withDatabaseConnection :: (AcidState UserAcid.UserList -> IO ()) -> IO ()
withDatabaseConnection = initDatabase User{ User.name="Foo", User.userId=0, calendarEntries=[], belongingTasks=[] }

spec :: Spec
spec =
    around withDatabaseConnection $
        context "User" $ do
          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  userList <- query c GetUserList
                  _ <- update c (NewUser "Mike")
                  userState <- query c $ UserAcid.UserById $ InterfaceAcid.nextEntryId userList
                  userState `shouldSatisfy` isJust
                  fromJust userState `shouldBe` User{ name="Mike", userId=InterfaceAcid.nextEntryId userList, calendarEntries=[], belongingTasks=[]}

              it "new and check InterfaceAcid.nextEntryId" $
                \c -> do
                  userList <- query c GetUserList
                  let oldId = InterfaceAcid.nextEntryId userList
                  _ <- update c (NewUser "Mike") 
                  userList <- query c GetUserList
                  InterfaceAcid.nextEntryId userList `shouldBe` oldId + 1
