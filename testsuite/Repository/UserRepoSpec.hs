module Repository.UserRepoSpec (spec) where

import Test.Hspec

import Data.Maybe           ( isJust, fromJust)
import Data.Acid            ( AcidState, openLocalState, closeAcidState, query )
import Control.Exception    ( bracket )
import Data.IxSet           ( IxSet(..), insert, empty )

import Repository.UserRepo as UserRepo
import Domain.User as User

--problem here is what we create a connection to our database
withDatabaseConnection :: (AcidState UserRepo.UserList -> IO ()) -> IO ()
withDatabaseConnection = 
    bracket (openLocalState UserRepo.UserList{
      nextUserId = 1,
      users = insert User{ User.name="Foo", User.userId=0 } empty })
            closeAcidState

spec :: Spec
spec =
    around withDatabaseConnection $
        describe "find user" $
            it "by id" $ \c -> do
                userState <- query c $ UserRepo.UserById 0
                userState `shouldSatisfy` isJust
                fromJust userState `shouldBe` User{ name="Foo", userId=0 }