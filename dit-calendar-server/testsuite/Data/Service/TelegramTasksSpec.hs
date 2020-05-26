{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.TelegramTasksSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Default                         (def)
import           Test.Hspec
import           Test.HUnit.Base                      (assertEqual)

import           Control.Monad.Identity               (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Class           (tell)
import           Data.List                            (delete)

import           Data.Domain.Task                     as Task
import           Data.Domain.TelegramLink             as TelegramLink
import           Data.Repository.CalendarRepo         (MonadDBCalendarRepo)
import           Data.Repository.TaskRepo             (MonadDBTaskRepo)
import           Data.Repository.TelegramLinkRepo     (MonadDBTelegramRepo)

import qualified Data.Service.TelegramTasks as TelegramTasksService


mkFixture "Fixture" [ts| MonadDBTaskRepo, MonadDBTelegramRepo |]

newTelegramLink = def{TelegramLink.chatId = 256, telegramUserId = 257, firstName = Just "foo", TelegramLink.owner=10 }
telegramLinkDB = def{TelegramLink.chatId = 352, telegramUserId = 353, TelegramLink.assignedToTasks = [10, 2], firstName = Just "bar", TelegramLink.owner=10}
taskFromDb = def{ Task.title="A", Task.description=Just "task1", taskId=10, startTime=Nothing, endTime=Nothing, assignedTelegramLinks=[1, TelegramLink.chatId telegramLinkDB], Task.owner=10}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture {
                    _findTelegramLinkById = \a -> tell [show a] >>= (\_ -> return $ Just telegramLinkDB)
                    , _updateTelegramLink = \tLink -> tell [show tLink] >>= (\_ -> return $ Right tLink)
                    , _updateTask  = \task -> tell [show task] >>= (\_ -> return $ Right task)
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "TelegramAssignmentSpec" $ do
    it "deleteTaskFromAllTelegramLinks" $ do
        let telegramLinkToUpdate = telegramLinkDB {assignedToTasks = delete (Task.taskId taskFromDb) (assignedToTasks telegramLinkDB)}
        let (_, log) = evalTestFixture (TelegramTasksService.deleteTaskFromAllTelegramLinksImpl taskFromDb) fixture
        length log `shouldBe` 4
        assertEqual "Nach falscher TelegramLink-Id gesucht" (log!!0) "1"
        assertEqual "falschen telegramLink für das update" (log!!1) (show telegramLinkToUpdate)
        assertEqual "Nach falscher TelegramLink-Id gesucht" (log!!2) (show $ TelegramLink.chatId telegramLinkDB)
        assertEqual "Task vom TelegramLink 2 nicht gelöscht" (log!!3) (show telegramLinkToUpdate)
    it "addTelegramLinkToTask" $ do
        let (_, log) = evalTestFixture (TelegramTasksService.addTelegramLinkToTaskImpl newTelegramLink taskFromDb) fixture
        let telegramLinkToUpdate = newTelegramLink {assignedToTasks = Task.taskId taskFromDb : assignedToTasks newTelegramLink}
        let expectedTask = taskFromDb {assignedTelegramLinks = TelegramLink.chatId newTelegramLink : assignedTelegramLinks taskFromDb}
        length log `shouldBe` 2
        assertEqual "update telegram link with wrong tasks in telegram link" (log!!0) (show telegramLinkToUpdate)
        assertEqual "task update with new telegram link failed" (log!!1) (show expectedTask)
    it "removeUserFromTask" $ do
        let telegramLinkToUpdate = telegramLinkDB {assignedToTasks = delete (Task.taskId taskFromDb) (assignedToTasks telegramLinkDB)}
        let expectedTask = taskFromDb {assignedTelegramLinks = delete (TelegramLink.chatId telegramLinkDB) (assignedTelegramLinks taskFromDb)}
        let (_, log) = evalTestFixture (TelegramTasksService.removeTelegramLinkFromTaskImpl taskFromDb telegramLinkDB) fixture
        length log `shouldBe` 2
        assertEqual "update telegram link with wrong tasks in telegram link" (log!!0) (show telegramLinkToUpdate)
        assertEqual "task update with new telegram link failed" (log!!1) (show expectedTask)
