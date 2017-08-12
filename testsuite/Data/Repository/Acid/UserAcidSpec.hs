module Data.Repository.Acid.UserAcidSpec (spec) where

import Test.Hspec
import Data.Maybe           ( isJust, fromJust, isNothing)
import Data.Acid            ( AcidState, query, update )
import Data.IxSet           ( IxSet(..), insert, empty )

import Data.Repository.Acid.DataBaseHelper   ( createDatabaseConnection )
import Data.Repository.Acid.UserAcid as UserAcid
import Data.Domain.User as User


withDatabaseConnection :: (AcidState UserAcid.UserList -> IO ()) -> IO ()
withDatabaseConnection = createDatabaseConnection UserAcid.UserList{
      nextUserId = 1,
      users      = insert User{ User.name="Foo", User.userId=0, calendarEntries=[], belongingTasks=[] } empty
    }

spec :: Spec
spec =
    around withDatabaseConnection $
        context "User" $ do
          describe "find" $
              it "by id" $
                \c -> do
                  userState <- query c $ UserAcid.UserById 0
                  userState `shouldSatisfy` isJust
                  fromJust userState `shouldBe` User{ name="Foo", userId=0, calendarEntries=[], belongingTasks=[] }

          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  userList <- query c GetUserList
                  _ <- update c (NewUser "Mike")
                  userState <- query c $ UserAcid.UserById $ nextUserId userList
                  userState `shouldSatisfy` isJust
                  fromJust userState `shouldBe` User{ name="Mike", userId=nextUserId userList, calendarEntries=[], belongingTasks=[]}

              it "new and check nextUserId" $
                \c -> do
                  userList <- query c GetUserList
                  let oldId = nextUserId userList
                  _ <- update c (NewUser "Mike") 
                  userList <- query c GetUserList
                  nextUserId userList `shouldBe` oldId + 1

          describe "delete" $
              it "create/delete user and check existence" $
                \ c -> do
                  userList <- query c GetUserList
                  _ <- update c (NewUser "Mike")
                  userState <- update c $ UserAcid.DeleteUser (nextUserId userList)
                  userList <- query c GetUserList
                  userState <- query c $ UserAcid.UserById (nextUserId userList)
                  userState `shouldSatisfy` isNothing

          describe "update" $
              it "update and check changes" $
                \c -> do
                  userList <- query c GetUserList
                  let x = nextUserId userList
                  _ <- update c (NewUser "Mike")
                  userState <- query c $ UserAcid.UserById x
                  let updatedUser = (fromJust userState) {name = "Alex"}
                  _ <- update c $ UserAcid.UpdateUser updatedUser
                  userState <- query c $ UserAcid.UserById x
                  userState `shouldSatisfy` isJust
                  fromJust userState `shouldBe` User{ name="Alex", userId=x, calendarEntries=[], belongingTasks=[]}
