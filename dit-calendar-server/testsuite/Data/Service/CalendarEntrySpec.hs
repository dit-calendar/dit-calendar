{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.CalendarEntrySpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Default                   (def)
import           Data.Text                      (unpack)
import           Test.Hspec

import           Control.Monad.Identity         (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Class     (tell)

import           Data.Domain.CalendarEntry      as CalendarEntry
import           Data.Domain.Task               as Task
import           Data.Domain.Types              (EntryId, TaskId, UserId)
import           Data.Domain.User               as User

import           Data.Repository.CalendarRepo   (MonadDBCalendarRepo)
import           Data.Repository.TaskRepo       (MonadDBTaskRepo)
import           Data.Repository.UserRepo       (MonadDBUserRepo)
import           Data.Time.Clock                (UTCTime)

import qualified Data.Service.CalendarEntry     as CalendarEntryService
import qualified Data.Service.Task              as TaskService
import qualified Data.Service.User              as UserService


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBTaskRepo, MonadDBCalendarRepo |]

userFromDb = def{ loginName="Foo", User.userId=10, ownerOfTasks=[1,2,3] }
taskFromDb = def { Task.description="task1", taskId=1, startTime=Nothing, endTime=Nothing}
dbDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
entryFromDb = def { CalendarEntry.description="termin2", entryId=1, CalendarEntry.owner=10, tasks=[1,2],
        startDate=dbDate, endDate=dbDate}
newDate = read "2012-11-19 17:51:42.203841 UTC"::UTCTime
newCalendar = def {CalendarEntry.startDate = newDate, CalendarEntry.endDate=dbDate,
    CalendarEntry.description ="termin2"}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _createCalendarEntry = \a -> tell [show a] >> return entryFromDb
                  , _findUserById = \a -> tell [show a] >>= (\_ -> return $ Just userFromDb)
                  , _findTaskById = \a -> tell [show a] >>= (\_ -> return $ Just taskFromDb)
                  , _addCalendarEntryToUser = \user entryId -> tell [show user] >> tell [show entryId] >>= (\_ -> return $ Right user)
                  , _deleteCalendarEntryFromUser = \user entryId -> tell [show user] >> tell [show entryId] >>= (\_ -> return $ Right user)
                  , _deleteTask = \a -> tell [show a]
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "CalendarEntryServiceSpec" $ do
    it "createEntry" $ do
        let user = def{ loginName="Foo", User.userId=10, ownerOfCalendarEntries=[1,2] }
        let (result, log) = evalTestFixture (CalendarEntryService.createEntryImpl newCalendar user) fixture
        result `shouldBe` entryFromDb
        -- Test calendarRepo calls
        log!!0 `shouldBe` show (newCalendar{CalendarEntry.owner=10})
        -- test UserRepo calls
        log!!1 `shouldBe` show user
        log!!2 `shouldBe` show (CalendarEntry.entryId entryFromDb)
    it "removeCalendar" $ do
        let calc = def { CalendarEntry.description="termin2", entryId=4, CalendarEntry.owner=2, tasks=[1]}
        let (_, log) = evalTestFixture (CalendarEntryService.removeCalendarImpl calc) fixture
        -- Test UserRepo calls
        log!!0 `shouldBe` show (CalendarEntry.owner calc)
        log!!1 `shouldBe` show userFromDb
        -- Test TaskRepo calls
        log!!2 `shouldBe` show (CalendarEntry.entryId calc)
        log!!3 `shouldBe` show (Task.taskId taskFromDb)
        -- Test CalendarRepo calls
        log!!4 `shouldBe` show (Task.taskId taskFromDb)
    it "getTasks" $ do
        let calc = def { CalendarEntry.description="termin2", entryId=4, CalendarEntry.owner=2, tasks=[1]}
        let (result, _) = evalTestFixture (CalendarEntryService.getCalendarTasksIml entryFromDb) fixture
        length result `shouldBe` 2

