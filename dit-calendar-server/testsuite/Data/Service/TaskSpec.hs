{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.TaskSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Default                 (def)
import           Data.Text                    (Text)
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
import           Data.Time.Clock              (UTCTime)

import qualified Data.Service.CalendarEntry   as CalendarEntryService
import qualified Data.Service.Task            as TaskService
import qualified Data.Service.User            as UserService


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBTaskRepo, MonadDBCalendarRepo |]

dbDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
userFromDb = def{ loginName="Foo", User.userId=10, belongingTasks=[1,2,3] }
taskFromDb = def{ Task.description="task1", taskId=5, startTime=Nothing, endTime=Nothing}
entryFromDb = def { CalendarEntry.description="termin2", entryId=1, CalendarEntry.userId=10,
        CalendarEntry.startDate=dbDate, CalendarEntry.endDate=dbDate,
        CalendarEntry.tasks = [1, 2]}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _addTaskToCalendarEntry = \entry taskId -> tell [show entry] >> tell [show taskId] >>= (\_ -> return $ Right entry)
                  , _updateTask = \a -> tell [show a] >>= (\_ -> return $ Right a)
                  , _deleteTask = \a -> tell [show a]
                  , _createTask = \a -> return taskFromDb
                  , _findTaskById = \a -> tell [show a] >>= (\_ -> return $ Just taskFromDb)
                  , _addTaskToUser = \user taskId -> tell [show user] >> tell [show taskId] >>= (\_ -> return $ Right user)
                  , _deleteTaskFromUser = \x a -> tell [show x] >> tell [show a] >>= (\_ -> return $ Right x)
                  , _findUserById = \a -> tell [show a] >>= (\_ -> return $ Just userFromDb)
                  , _deleteTaskFromCalendarEntry = \c i -> tell [show c] >> tell [show i] >>= (\_ -> return $ Right undefined)
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "TaskServiceSpec" $ do
    it "deleteTaskAndCascade" $ do
        let task = def{ Task.description="task1", taskId=1, belongingUsers=[7], startTime=Nothing, endTime=Nothing}
        let (_, log) = evalTestFixture (TaskService.deleteTaskAndCascadeImpl entryFromDb task) fixture
        log!!0 `shouldBe` show entryFromDb
        log!!1 `shouldBe` show (Task.taskId task)
        log!!2 `shouldBe` "7"
        log!!3 `shouldBe` show userFromDb
        log!!4 `shouldBe` "7"
        log!!5 `shouldBe` show (Task.taskId task)
    it "createTaskInCalendar" $ do
        let newDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
        let calc = def{ CalendarEntry.description="termin2", entryId=1, CalendarEntry.userId=2,
            startDate=newDate, endDate=newDate}
        let (result, log) = evalTestFixture (TaskService.createTaskInCalendarImpl calc taskFromDb) fixture
        result `shouldBe` taskFromDb
        log!!0 `shouldBe` show calc
        log!!1 `shouldBe` show (Task.taskId taskFromDb)
    it "addUserToTask" $ do
        let task = def { Task.description="task1", taskId=1, belongingUsers=[2], startTime=Nothing, endTime=Nothing}
        let expectedTask = def { Task.description="task1", taskId=1, belongingUsers=[10, 2], startTime=Nothing, endTime=Nothing}
        let (_, log) = evalTestFixture (TaskService.addUserToTaskImpl task userFromDb) fixture
        log!!0 `shouldBe` show userFromDb
        log!!1 `shouldBe` show (Task.taskId task)
        log!!2 `shouldBe` show expectedTask
    it "removeUserFromTask" $ do
        let task = def { Task.description="task1", taskId=1, belongingUsers=[2,10], startTime=Nothing, endTime=Nothing}
        let expectedTask = def { Task.description="task1", taskId=1, belongingUsers=[2], startTime=Nothing, endTime=Nothing}
        let (_, log) = evalTestFixture (TaskService.removeUserFromTaskImpl task userFromDb) fixture
        log!!0 `shouldBe` show userFromDb
        log!!1 `shouldBe` show (Task.taskId task)
        log!!2 `shouldBe` show expectedTask
