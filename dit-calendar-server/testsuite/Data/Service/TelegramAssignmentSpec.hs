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
import           Data.Domain.User                     as User
import           Data.Repository.CalendarRepo         (MonadDBCalendarRepo)
import           Data.Repository.TaskRepo             (MonadDBTaskRepo)

import qualified Data.Service.TelegramTasksAssignment as TelegramTasksService


mkFixture "Fixture" [ts| MonadDBTaskRepo |]


taskFromDb = def{ Task.description="task1", taskId=5, startTime=Nothing, endTime=Nothing, assignedUsers=[8,11]}
taskFromDb2 = def{ Task.description="task2", taskId=6, startTime=Nothing, endTime=Nothing, assignedTelegramLinks=[8]}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture {
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "TelegramAssignmentSpec" $ do
    it "deleteTaskFromAllUsers" $ do
        let task = def{ Task.description="task1", taskId=1, assignedTelegramLinks=[7,8], startTime=Nothing, endTime=Nothing}
        let (_, log) = evalTestFixture (TelegramTasksService.deleteTaskFromAllTelegramLinksImpl task) fixture
        length log `shouldBe` 2
        -- deleteTaskFromUser calls
--        assertEqual "deleteTaskFromUser callend with wrong user or task" (log!!0) (show userFromDb ++ show task)
--        assertEqual "deleteTaskFromUser callend with wrong user or task" (log!!1) (show userFromDb2 ++ show task)
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
