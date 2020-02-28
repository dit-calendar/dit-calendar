{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.TelegramAssignmentSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Default                         (def)
import           Test.Hspec
import           Test.HUnit.Base                      (assertEqual)

import           Control.Monad.Identity               (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Class           (tell)

import           Data.Domain.Task                     as Task
import           Data.Domain.TelegramLink             as TelegramLink
import           Data.Repository.CalendarRepo         (MonadDBCalendarRepo)
import           Data.Repository.TaskRepo             (MonadDBTaskRepo)
import           Data.Repository.TelegramLinkRepo     (MonadDBTelegramRepo)

import qualified Data.Service.TelegramTasksAssignment as TelegramTasksService


mkFixture "Fixture" [ts| MonadDBTaskRepo, MonadDBTelegramRepo |]

telegramLinkDB = def{TelegramLink.telegramUserId = 1}
taskFromDb = def{ Task.description="task1", taskId=1, startTime=Nothing, endTime=Nothing, assignedTelegramLinks=[1,2]}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture {
                    _findTelegramLinkById = \a -> tell [show a] >>= (\_ -> return $ Just telegramLinkDB)
                    , _deleteTaskFromTelegramLink = \tLink task -> tell [show tLink] >> tell [show task] >>= (\_ -> return $ Right tLink)
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "TelegramAssignmentSpec" $ do
    it "deleteTaskFromAllTelegramLinks" $ do
        let (_, log) = evalTestFixture (TelegramTasksService.deleteTaskFromAllTelegramLinksImpl taskFromDb) fixture
        length log `shouldBe` 6
        assertEqual "Nach falscher TelegramLink-Id gesucht" (log!!0) "1"
        assertEqual "TelegramLink 1 nicht an deleteTaskFromTelegramLink durchgegeben" (log!!1) (show telegramLinkDB)
        assertEqual "Task nicht an deleteTaskFromTelegramLink durchgegeben" (log!!2) (show taskFromDb)
        assertEqual "Nach falscher TelegramLink-Id gesucht" (log!!3) "2"
        assertEqual "Task vom TelegramLink 2 nicht gelöscht" (log!!4) (show telegramLinkDB)
        assertEqual "Task nicht an deleteTaskFromTelegramLink durchgegeben" (log!!5) (show taskFromDb)
    it "addUserToTask" $ do
        let task = def { Task.description="task1", taskId=1, assignedTelegramLinks=[2], startTime=Nothing, endTime=Nothing}
        let expectedTask = def { Task.description="task1", taskId=1, assignedTelegramLinks=[10, 2], startTime=Nothing, endTime=Nothing}
        let (_, log) = evalTestFixture (TelegramTasksService.addTelegramLinkToTaskImpl task undefined) fixture
        length log `shouldBe` 2
--        assertEqual "addTaskToUser callend with wrong user or task" (log!!0) (show userFromDb ++ show (Task.taskId task))
--        assertEqual "task update with new user failed" (log!!1) (show expectedTask)
    it "removeUserFromTask" $ do
        let task = def { Task.description="task1", taskId=1, assignedTelegramLinks=[2,10], startTime=Nothing, endTime=Nothing}
        let expectedTask = def { Task.description="task1", taskId=1, assignedTelegramLinks=[2], startTime=Nothing, endTime=Nothing}
        let (_, log) = evalTestFixture (TelegramTasksService.removeTelegramLinkFromTaskImpl task undefined) fixture
        length log `shouldBe` 2
--        assertEqual "deleteTaskFromUser callend with wrong user or task" (log!!0) (show userFromDb ++ show task)
--        assertEqual "update task without old user" (log!!01) (show expectedTask)
