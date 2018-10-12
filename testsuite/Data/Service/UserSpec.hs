{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Service.UserSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Test.Hspec
import           Data.Text                    (Text)
import           Test.HUnit.Base              (assertEqual)


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
        assertEqual "CalendarEntry 1 nicht durchgegeben" (log!!0) "1"
        assertEqual "CalendarEntry 2 nicht durchgegeben" (log!!1) "2"
        assertEqual "Taskeintrag aus calendar nicht gelöscht" (log!!2) "4"
        assertEqual "Task nicht gelöscht" (log!!3) (show taskFromDb)
        assertEqual "Falsche userId durchgegeben" (log!!4) (show $User.userId userFromDb)
        assertEqual "Falscher user gelöscht" (log!!5) (show userFromDb)
