{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Service.UserServiceSpec (spec) where

import Test.Hspec
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

import Control.Monad.IO.Class
import Control.Monad.Identity        ( Identity )
import Control.Monad.Writer.Class    ( tell )

import Data.Domain.CalendarEntry  as CalendarEntry
import Data.Domain.User           as User
import Data.Domain.Task           as Task
import Data.Domain.Types          ( UserId, EntryId, TaskId )

import Data.Repository.MonadDB.User          ( MonadDBUserRepo )
import Data.Repository.MonadDB.Task          ( MonadDBTaskRepo )
import Data.Repository.MonadDB.Calendar      ( MonadDBCalendarRepo )

import qualified Data.Service.User          as UserService
import qualified Data.Service.Task          as TaskService
import qualified Data.Service.CalendarEntry as CalendarEntryService


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBTaskRepo, MonadDBCalendarRepo |]

userFromDb = User{ name="Foo", User.userId=10, calendarEntries=[1,2], belongingTasks=[4] }
taskFromDb = Task{ Task.description="task1", taskId=5, belongingUsers=[10]}

fixture = Fixture { _deleteCalendarEntry = \(a) -> tell [show a]
                  , _getTask = \(a) -> tell [show a] >> return taskFromDb
                  , _deleteUser = \(a) -> tell [show a]
                  , _deleteTaskFromUser = \x a -> tell [show x] >> tell [show a]
                  , _getUser = \(a) -> tell [show a] >> return userFromDb
                  , _updateTask = \(a) -> tell [show a]
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "UserService" $ do
    it "deleteUser" $ do
        let expectedTask = Task{ Task.description="task1", taskId=5, belongingUsers=[]}
        let (_, log) = evalTestFixture (UserService.deleteUser userFromDb) fixture
        log!!0 `shouldBe` "1"
        log!!1 `shouldBe` "2"
        log!!2 `shouldBe` "4"
        log!!3 `shouldBe` (show (User.userId userFromDb))
        log!!4 `shouldBe` (show userFromDb)
        log!!5 `shouldBe` (show (Task.taskId taskFromDb))
        log!!6 `shouldBe` (show expectedTask)
        log!!7 `shouldBe` (show userFromDb)