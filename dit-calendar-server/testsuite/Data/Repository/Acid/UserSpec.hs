{-# LANGUAGE OverloadedStrings #-}

module Data.Repository.Acid.UserSpec (spec) where

import           Data.Acid                           (AcidState, query, update)
import           Data.Default                        (def)
import           Data.Maybe                          (fromJust, isJust,
                                                      isNothing)
import           Test.Hspec

import           Data.Domain.User                    as User
import           Data.Repository.Acid.DataBaseHelper (initDatabase)

import qualified Data.Repository.Acid.InterfaceAcid  as InterfaceAcid
import qualified Data.Repository.Acid.User           as UserAcid

withDatabaseConnection :: (AcidState UserAcid.UserList -> IO ()) -> IO ()
withDatabaseConnection = initDatabase 1 def{ User.loginName="Foo", userId=0}

spec :: Spec
spec =
    around withDatabaseConnection $
        context "User" $
          describe "find" $ do
              it "by Username" $
                \c -> do
                  userState   <- query c $ UserAcid.FindByLoginName "Foo"
                  userState `shouldSatisfy` isJust
                  fromJust userState `shouldBe` def{  User.loginName="Foo", userId=0 }
              it "by wrong Username" $
                  \c -> do
                    userState   <- query c $ UserAcid.FindByLoginName "Foo1"
                    userState `shouldSatisfy` isNothing
