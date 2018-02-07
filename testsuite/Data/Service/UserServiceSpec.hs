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


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBTaskRepo, MonadDBCalendarRepo |]

userFromDb = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[1,2,3] }
taskFromDb = Task{ Task.description="task1", taskId=1, belongingUsers=[]}

fixture = Fixture { _newCalendarEntry = undefined
                  , _deleteCalendarEntry = \(a) -> tell (show a)
                  , _deleteTaskFromCalendarEntry = undefined
                  , _addTaskToCalendarEntry = undefined
                  , _updateTask = undefined
                  , _deleteTask = undefined
                  , _createTask = undefined
                  , _getTask = undefined
                  , _createUser = undefined
                  , _deleteUser = \(a) -> tell (show a)
                  , _updateName = undefined
                  , _addCalendarEntryToUser = undefined
                  , _deleteCalendarEntryFromUser = undefined
                  , _addTaskToUser = undefined
                  , _deleteTaskFromUser = undefined
                  , _getUser = undefined
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "UserService" $ do
    it "deleteUser" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[1,2], belongingTasks=[] }
        let (_, log) = evalTestFixture (UserService.deleteUser user) fixture
        log `shouldBe` ("12" ++ show user)