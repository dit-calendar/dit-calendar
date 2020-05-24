{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Repository.UserRepoSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Test.Hspec

import           Control.Monad.Identity       (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Class   (tell)
import           Data.Default                 (def)
import           Data.Maybe                   (fromJust)

import           Data.Domain.Task             as Task
import           Data.Domain.User             as User
import           Data.Repository.Acid.User    (DeleteUser (..),
                                               FindByLoginName (..),
                                               NewUser (..), UpdateUser (..),
                                               UserById (..), UserDAO)

import qualified Data.Repository.UserRepo     as UserRepo


mkFixture "Fixture" [ts| UserDAO |]

userFromDb = def { loginName="Foo", User.userId=10}
taskFromDb = def {Task.taskId=2}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _create = \(NewUser a)    -> return a
                  , _delete = \(DeleteUser a) -> tell [show a]
                  , _update = \(UpdateUser a) -> tell [show a] >>= (\_ -> return $ Right a)
                  , _query = \(UserById a)    -> return (Just userFromDb)
                  , _queryByLoginName = \(FindByLoginName a)    -> return (Just userFromDb) }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "UserRepo" $ do
    it "createUser" $ do
        let user = def { loginName="name" }
        let (result, _) = evalTestFixture (UserRepo.createUserImpl user) fixture
        User.loginName result `shouldBe` "name"
        User.ownerOfCalendarEntries result `shouldBe` []
    it "deleteUser" $ do
        let user = def { loginName="Foo", User.userId=10}
        let (_, log) = evalTestFixture (UserRepo.deleteUserImpl user) fixture
        log `shouldBe` ["10"::String]
    it "updateName" $ do
        let user = def { loginName="Foo", User.userId=10 }
        let (_, log) = evalTestFixture (UserRepo.updateUserImpl user {loginName = "Name2"}) fixture
        let newUser = user { loginName = "Name2" }
        log!!0 `shouldBe` show newUser
    it "addCalendarEntryToUser" $ do
        let user = def { loginName="Foo", User.userId=10, ownerOfCalendarEntries=[1]}
        let (_, log) = evalTestFixture (UserRepo.addCalendarEntryToUserImpl user 2) fixture
        let newUser = user {ownerOfCalendarEntries = [2, 1]}
        log!!0 `shouldBe` show newUser
    it "deleteCalendarEntryFromUser" $ do
        let user = def{ loginName="Foo", User.userId=10, ownerOfCalendarEntries=[1,2,3]}
        let (_, log) = evalTestFixture (UserRepo.deleteCalendarEntryFromUserImpl user 2) fixture
        let newUser = user {ownerOfCalendarEntries = [1, 3]}
        log!!0 `shouldBe` show newUser
    describe "find" $ do
        it "byId" $ do
            let (result, _) = evalTestFixture (UserRepo.findUserByIdImpl 10) fixture
            result `shouldBe` Just userFromDb
        it "byName" $ do
            let (result, _) = evalTestFixture (UserRepo.findUserByLoginNameIml "Foo") fixture
            fromJust result `shouldBe` userFromDb
