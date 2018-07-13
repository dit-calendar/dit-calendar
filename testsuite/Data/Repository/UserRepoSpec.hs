{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Repository.UserRepoSpec (spec) where

import Test.Hspec
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

import Data.Maybe                    (fromJust)
import Control.Monad.IO.Class
import Control.Monad.Identity        ( Identity )
import Control.Monad.Writer.Class    ( tell )

import Data.Repository.Acid.User          ( MonadDBUser )
import Data.Repository.Acid.User          ( NewUser(..), DeleteUser(..), UpdateUser(..), UserById(..), FindByName(..) )
import Data.Domain.User                     as User

import qualified Data.Repository.UserRepo          as UserRepo


mkFixture "Fixture" [ts| MonadDBUser |]

userFromDb = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[1,2,3] }

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _create = \(NewUser a)    -> return a
                  , _delete = \(DeleteUser a) -> tell [show a]
                  , _update = \(UpdateUser a) -> tell [show a]
                  , _query = \(UserById a)    -> return (Just userFromDb)
                  , _queryByName = \(FindByName a)    -> return (Just userFromDb) }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "UserRepo" $ do
    it "createUser" $ do
        let (result, _) = evalTestFixture (UserRepo.createUserImpl "name") fixture
        User.name result `shouldBe` "name"
        User.calendarEntries result `shouldBe` []
        User.belongingTasks result `shouldBe` []
    it "deleteUser" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[] }
        let (_, log) = evalTestFixture (UserRepo.deleteUserImpl user) fixture
        log `shouldBe` ["10"::String]
    it "updateName" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[] }
        let (_, log) = evalTestFixture (UserRepo.updateNameImpl user "Name2") fixture
        let newUser = user { name = "Name2" }
        log!!0 `shouldBe` show newUser
    it "addCalendarEntryToUser" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[1], belongingTasks=[] }
        let (_, log) = evalTestFixture (UserRepo.addCalendarEntryToUserImpl user 2) fixture
        let newUser = user {calendarEntries = [1, 2]}
        log!!0 `shouldBe` show newUser
    it "deleteCalendarEntryFromUser" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[1,2,3], belongingTasks=[] }
        let (_, log) = evalTestFixture (UserRepo.deleteCalendarEntryFromUserImpl user 2) fixture
        let newUser = user {calendarEntries = [1, 3]}
        log!!0 `shouldBe` show newUser
    it "addTaskToUser" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[1] }
        let (_, log) = evalTestFixture (UserRepo.addTaskToUserImpl user 2) fixture
        let newUser = user {belongingTasks = [1, 2]}
        log!!0 `shouldBe` show newUser
    it "deleteTaskFromUser" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[1,2,3] }
        let (_, log) = evalTestFixture (UserRepo.deleteTaskFromUserImpl user 2) fixture
        let newUser = user {belongingTasks = [1, 3]}
        log!!0 `shouldBe` show newUser
    describe "find" $ do
        it "byId" $ do
            let (result, _) = evalTestFixture (UserRepo.getUserImpl 10) fixture
            result `shouldBe` userFromDb
        it "byName" $ do
            let (result, _) = evalTestFixture (UserRepo.findUserByNameIml "Foo") fixture
            fromJust result `shouldBe` userFromDb
