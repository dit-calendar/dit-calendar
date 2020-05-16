{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.CalendarTasksSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Control.Monad.Writer.Class   (tell)
import           Data.Default                 (def)
import           Test.Hspec

import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.Task             as Task

import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import           Data.Time.Clock              (UTCTime)

import qualified Data.Service.CalendarTasks   as CalendarTasks


mkFixture "Fixture" [ts| MonadDBTaskRepo |]

taskFromDb1 = def { Task.title="A", Task.description=Just "task1", taskId=1, startTime=Nothing, endTime=Nothing, Task.owner=10}
taskFromDb2 = def { Task.title="A", Task.description=Just "task2", taskId=2, startTime=Nothing, endTime=Nothing, Task.owner=10}
dbDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
entryFromDb = def { CalendarEntry.title="A", CalendarEntry.description=Just "termin2", entryId=1, CalendarEntry.owner=10, tasks=[1,2],
        startDate=dbDate, endDate=dbDate}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _findTaskById = \a -> return $ Just (if a == 1 then taskFromDb1 else taskFromDb2)
                    ,_deleteTask = \a -> tell [show a]
                  }


spec = describe "CalendarEntryServiceSpec" $ do
    it "deleteCalendarsTasks" $ do
        let (_, log) = evalTestFixture (CalendarTasks.deleteCalendarsTasksImpl entryFromDb) fixture
        -- Test TaskRepo calls
        length log `shouldBe` 2
        log!!0 `shouldBe` show taskFromDb1
        log!!1 `shouldBe` show taskFromDb2
    it "getTasks" $ do
        let (result, _) = evalTestFixture (CalendarTasks.getCalendarTasksIml entryFromDb) fixture
        length result `shouldBe` 2
