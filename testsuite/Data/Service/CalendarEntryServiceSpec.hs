{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Service.CalendarEntryServiceSpec (spec) where

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

userFromDb = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[1,2,3] }
taskFromDb = Task{ Task.description="task1", taskId=1, belongingUsers=[]}
entryFromDb = CalendarEntry{ CalendarEntry.description="termin2", entryId=1, CalendarEntry.userId=2, tasks=[]}

fixture = Fixture { _newCalendarEntry = \description user -> tell [description] >> tell [show user] >> return entryFromDb
                  , _getUser = \(a) -> tell [show a] >> return userFromDb
                  , _getTask = \(a) -> tell [show a] >> return taskFromDb
                  , _addCalendarEntryToUser = \user entryId -> tell [show user] >> tell [show entryId]
                  , _deleteCalendarEntryFromUser = \user entryId -> tell [show user] >> tell [show entryId]
                  , _deleteTask = \(a) -> tell [show a]
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "CalendarEntryServiceSpec" $ do
    it "createEntry" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[1,2], belongingTasks=[] }
        let (result, log) = evalTestFixture (CalendarEntryService.createEntry "termin2" user) fixture
        result `shouldBe` entryFromDb
        log!!0 `shouldBe` "termin2"
        log!!1 `shouldBe` (show user)
        log!!2 `shouldBe` (show user)
        log!!3 `shouldBe` (show (CalendarEntry.entryId entryFromDb))
    it "removeCalendar" $ do
        let calc = CalendarEntry{ CalendarEntry.description="termin2", entryId=4, CalendarEntry.userId=2, tasks=[1]}
        let (_, log) = evalTestFixture (CalendarEntryService.removeCalendar calc) fixture
        log!!0 `shouldBe` (show (CalendarEntry.userId calc))
        log!!1 `shouldBe` (show userFromDb)
        log!!2 `shouldBe` (show (CalendarEntry.entryId calc))
        log!!3 `shouldBe` (show (Task.taskId taskFromDb))
        log!!4 `shouldBe` (show taskFromDb)