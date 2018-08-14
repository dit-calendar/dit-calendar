{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.UserSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Test.Hspec

import           Control.Monad.Identity       (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Class   (tell)

import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.Task             as Task
import           Data.Domain.Types            (EntryId, TaskId, UserId)
import           Data.Domain.User             as User

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import           Data.Service.Task            (TaskService)

import qualified Data.Service.CalendarEntry   as CalendarEntryService
import qualified Data.Service.Task            as TaskService
import qualified Data.Service.User            as UserService


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBTaskRepo, MonadDBCalendarRepo, TaskService |]

userFromDb = User{ name="Foo", User.userId=10, calendarEntries=[1,2], belongingTasks=[4] }
taskFromDb = Task{ Task.description="task1", taskId=5, belongingUsers=[10]}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _deleteCalendarEntry = \(a) -> tell [show a]
                  , _getTask = \(a) -> tell [show a] >> return taskFromDb
                  , _deleteUser = \(a) -> tell [show a]
                  , _deleteTaskFromUser = \x a -> tell [show x] >> tell [show a]
                  , _getUser = \(a) -> tell [show a] >> return userFromDb
                  , _updateTask = \(a) -> tell [show a]
                  , _removeUserFromTask =  \x a -> tell [show x] >> tell [show a]
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "UserService" $
    it "deleteUser" $ do
        let expectedTask = Task{ Task.description="task1", taskId=5, belongingUsers=[]}
        let (_, log) = evalTestFixture (UserService.deleteUserImpl userFromDb) fixture
        log!!0 `shouldBe` "1"
        log!!1 `shouldBe` "2"
        log!!2 `shouldBe` "4"
        log!!3 `shouldBe` show taskFromDb
        log!!4 `shouldBe` show (User.userId userFromDb)
        log!!5 `shouldBe` show userFromDb
