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


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBTaskRepo, MonadDBCalendarRepo |]

userFromDb = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[1,2,3] }
taskFromDb = Task{ Task.description="task1", taskId=1, belongingUsers=[]}

fixture = Fixture { _newCalendarEntry = undefined
                  , _deleteCalendarEntry = \(a) -> tell [show a]
                  , _deleteTaskFromCalendarEntry = undefined
                  , _addTaskToCalendarEntry = \entry taskId -> tell [show entry] >> tell [show taskId]
                  , _updateTask = \(a) -> tell [show a]
                  , _deleteTask = \(a) -> tell [show a]
                  , _createTask = \(a) -> return taskFromDb
                  , _getTask = undefined
                  , _createUser = undefined
                  , _deleteUser = \(a) -> tell [show a]
                  , _updateName = undefined
                  , _addCalendarEntryToUser = undefined
                  , _deleteCalendarEntryFromUser = undefined
                  , _addTaskToUser = \user taskId -> tell [show user] >> tell [show taskId]
                  , _deleteTaskFromUser = \x a -> tell [show x] >> tell [show a]
                  , _getUser = \(a) -> return userFromDb
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "RepositoryService" $ do
    it "UserService.deleteUser" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[1,2], belongingTasks=[] }
        let (_, log) = evalTestFixture (UserService.deleteUser user) fixture
        log!!0 `shouldBe` "1"
        log!!1 `shouldBe` "2"
        log!!2 `shouldBe` (show user)
    it "TaskService.deleteTask" $ do
        let task = Task{ Task.description="task1", taskId=1, belongingUsers=[1]}
        let (_, log) = evalTestFixture (TaskService.deleteTask task) fixture
        log!!0 `shouldBe` (show userFromDb)
        log!!1 `shouldBe` "1"
        log!!2 `shouldBe` (show task)
    it "TaskService.createTask" $ do
        let calc = CalendarEntry{ CalendarEntry.description="termin2", entryId=1, CalendarEntry.userId=2, tasks=[]}
        let (result, log) = evalTestFixture (TaskService.createTask calc "task1") fixture
        result `shouldBe` taskFromDb
        log!!0 `shouldBe` (show calc)
        log!!1 `shouldBe` (show (Task.taskId taskFromDb))
    it "TaskService.addUserToTask" $ do
        let task = Task{ Task.description="task1", taskId=1, belongingUsers=[2]}
        let expectedTask = Task{ Task.description="task1", taskId=1, belongingUsers=[2,10]}
        let (_, log) = evalTestFixture (TaskService.addUserToTask task 10) fixture
        log!!0 `shouldBe` (show userFromDb)
        log!!1 `shouldBe` (show (Task.taskId task))
        log!!2 `shouldBe` (show expectedTask)

