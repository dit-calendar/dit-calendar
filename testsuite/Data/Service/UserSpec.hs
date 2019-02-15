{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.UserSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Default                 (def)
import           Data.Text                    (Text)
import           Test.Hspec
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

userFromDb = def { loginName="Foo", User.userId=10, calendarEntries=[1,2], belongingTasks=[4] }
taskFromDb = def { Task.description="task1", taskId=5, belongingUsers=[10], startTime=Nothing, endTime=Nothing}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _deleteCalendarEntry = \a -> tell [show a]
                  , _findTaskById = \a -> tell [show a] >>= (\_ -> return $ Just taskFromDb)
                  , _deleteUser = \a -> tell [show a]
                  , _deleteTaskFromUser = \x a -> tell [show x] >> tell [show a] >>= (\_ -> return $ Right x)
                  , _findUserById = \a -> tell [show a] >>= (\_ -> return $ Just userFromDb)
                  , _updateTask = \a -> tell [show a] >>= (\_ -> return $ Right a)
                  , _removeUserFromTask =  \x a -> tell [show x] >> tell [show a] >>= (\_ -> return $ Right x)
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "UserService" $
    it "deleteUser" $ do
        let expectedTask = def { Task.description="task1", taskId=5, startTime=Nothing, endTime=Nothing}
        let (_, log) = evalTestFixture (UserService.deleteUserImpl userFromDb) fixture
        assertEqual "CalendarEntry 1 nicht durchgegeben" (log!!0) "1"
        assertEqual "CalendarEntry 2 nicht durchgegeben" (log!!1) "2"
        assertEqual "Taskeintrag aus calendar nicht gelöscht" (log!!2) "4"
        assertEqual "Task nicht gelöscht" (log!!3) (show taskFromDb)
        assertEqual "Falsche userId durchgegeben" (log!!4) (show $ userFromDb)
        assertEqual "Falscher user gelöscht" (log!!5) (show $ User.userId userFromDb)
