{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Service.TaskServiceSpec (spec) where

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

import Data.Repository.UserRepo          ( MonadDBUserRepo )
import Data.Repository.TaskRepo              ( MonadDBTaskRepo )
import Data.Repository.CalendarRepo      ( MonadDBCalendarRepo )

import qualified Data.Service.User          as UserService
import qualified Data.Service.Task          as TaskService
import qualified Data.Service.CalendarEntry as CalendarEntryService


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBTaskRepo, MonadDBCalendarRepo |]

userFromDb = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[1,2,3] }
taskFromDb = Task{ Task.description="task1", taskId=5, belongingUsers=[]}

fixture = Fixture { _addTaskToCalendarEntry = \entry taskId -> tell [show entry] >> tell [show taskId]
                  , _updateTask = \(a) -> tell [show a]
                  , _deleteTask = \(a) -> tell [show a]
                  , _createTask = \(a) -> return taskFromDb
                  , _getTask = \(a) -> tell [show a] >> return taskFromDb
                  , _addTaskToUser = \user taskId -> tell [show user] >> tell [show taskId]
                  , _deleteTaskFromUser = \x a -> tell [show x] >> tell [show a]
                  , _getUser = \(a) -> tell [show a] >> return userFromDb
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "TaskServiceSpec" $ do
    it "deleteTask" $ do
        let task = Task{ Task.description="task1", taskId=1, belongingUsers=[7]}
        let (_, log) = evalTestFixture (TaskService.deleteTask task) fixture
        log!!0 `shouldBe` "7"
        log!!1 `shouldBe` (show userFromDb)
        log!!2 `shouldBe` "7"
        log!!3 `shouldBe` (show task)
    it "createTask" $ do
        let calc = CalendarEntry{ CalendarEntry.description="termin2", entryId=1, CalendarEntry.userId=2, tasks=[]}
        let (result, log) = evalTestFixture (TaskService.createTask calc "task1") fixture
        result `shouldBe` taskFromDb
        log!!0 `shouldBe` (show calc)
        log!!1 `shouldBe` (show (Task.taskId taskFromDb))
    it "addUserToTask" $ do
        let task = Task{ Task.description="task1", taskId=1, belongingUsers=[2]}
        let expectedTask = Task{ Task.description="task1", taskId=1, belongingUsers=[2,10]}
        let (_, log) = evalTestFixture (TaskService.addUserToTask task 10) fixture
        log!!0 `shouldBe` (show (User.userId userFromDb))
        log!!1 `shouldBe` (show userFromDb)
        log!!2 `shouldBe` (show (Task.taskId task))
        log!!3 `shouldBe` (show expectedTask)
    it "removeUserFromTask" $ do
        let task = Task{ Task.description="task1", taskId=1, belongingUsers=[2,10]}
        let expectedTask = Task{ Task.description="task1", taskId=1, belongingUsers=[2]}
        let (_, log) = evalTestFixture (TaskService.removeUserFromTask task 10) fixture
        log!!0 `shouldBe` (show (User.userId userFromDb))
        log!!1 `shouldBe` (show userFromDb)
        log!!2 `shouldBe` (show (Task.taskId task))
        log!!3 `shouldBe` (show expectedTask)