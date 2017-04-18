module Repository.UserRepoSpec (spec) where

import Test.Hspec

import Data.Maybe           ( isJust, fromJust, isNothing)
import Data.Acid            ( AcidState, openLocalState, closeAcidState, query, update )
import Control.Exception    ( bracket )
import Data.IxSet           ( IxSet(..), insert, empty )

import Repository.UserRepo as UserRepo
import Domain.User as User

--problem here is what we create a connection to our database
withDatabaseConnection :: (AcidState UserRepo.UserList -> IO ()) -> IO ()
withDatabaseConnection = 
    bracket (openLocalState UserRepo.UserList{
      nextUserId = 1,
      users = insert User{ User.name="Foo", User.userId=0, calendarEntrys=[] } empty })
            closeAcidState

spec :: Spec
spec =
    around withDatabaseConnection $
        context "User" $ do
          describe "find" $ do
              it "by id" $
                \c -> do
                  userState <- query c $ UserRepo.UserById 0
                  userState `shouldSatisfy` isJust
                  fromJust userState `shouldBe` User{ name="Foo", userId=0, calendarEntrys=[] }

          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  userList <- query c GetUserList
                  _ <- update c (NewUser "Mike")
                  userState <- query c $ UserRepo.UserById $ nextUserId userList
                  userState `shouldSatisfy` isJust
                  fromJust userState `shouldBe` User{ name="Mike", userId=nextUserId userList, calendarEntrys=[]}

              it "new and check nextUserId" $
                \c -> do
                  userList <- query c GetUserList
                  let oldId = nextUserId userList
                  _ <- update c (NewUser "Mike") 
                  userList <- query c GetUserList
                  nextUserId userList `shouldBe` oldId + 1
         
          describe "delete" $ do
              it "create/delete user and check existence" $
                \ c -> do 
                  userList <- query c GetUserList
                  _ <- update c (NewUser "Mike")
                  userState <- update c $ UserRepo.DeleteUser (nextUserId userList)
                  userList <- query c GetUserList
                  userState <- query c $ UserRepo.UserById (nextUserId userList)
                  userState `shouldSatisfy` isNothing