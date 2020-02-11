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

import           Data.Domain.Task             as Task
import           Data.Domain.User             as User

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import           Data.Service.TelegramTasksAssignment       (UserTasksService)

import qualified Data.Service.User            as UserService


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBCalendarRepo, UserTasksService |]

userFromDb = def { loginName="Foo", User.userId=10, ownerOfCalendarEntries=[1,2], assignedToTasks=[4] }

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _deleteCalendarEntryById = \a -> tell [show a]
                  , _deleteUser = \a -> tell [show a]
                  , _removeUserFromTasks = \a -> tell [show a]
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "UserService" $
    it "deleteUser" $ do
        let expectedTask = def { Task.description="task1", taskId=5, startTime=Nothing, endTime=Nothing}
        let (_, log) = evalTestFixture (UserService.deleteUserImpl userFromDb) fixture
        length log `shouldBe` 4
        assertEqual "removeUserFromTasks mit falschen User " (log!!0) (show userFromDb)
        assertEqual "CalendarEntry 1 nicht durchgegeben" (log!!1) "1"
        assertEqual "CalendarEntry 2 nicht durchgegeben" (log!!2) "2"
        assertEqual "Falscher user gelöscht" (log!!3) (show userFromDb)
