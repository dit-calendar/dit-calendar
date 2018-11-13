{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Repository.TaskRepoSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Test.Hspec

import           Control.Monad.Identity       (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer         (execWriter)
import           Control.Monad.Writer.Class   (tell)

import           Data.Domain.Task             as Task
import           Data.Repository.Acid.Task    (DeleteTask (..), NewTask (..),
                                               TaskById (..), TaskDAO,
                                               UpdateTask (..))

import qualified Data.Repository.TaskRepo     as TaskRepo


mkFixture "Fixture" [ts| TaskDAO |]

taskFromDb = Task{ description="task1", taskId=1, belongingUsers=[]}

fixture :: (Monad m, MonadWriter String m) => Fixture m
fixture = Fixture { _create = \(NewTask task) -> return task
                  , _delete = \(DeleteTask a) -> tell (show a)
                  , _update = \(UpdateTask a) -> tell (show a)
                  , _query  = \(TaskById a)   -> return (Just taskFromDb)}

instance MonadIO Identity where
    liftIO = undefined

spec = describe "CalendarRepo" $ do
    it "createTask" $ do
        let (result, _) = evalTestFixture (TaskRepo.createTaskImpl "task1") fixture
        Task.description result `shouldBe` "task1"
        Task.belongingUsers result `shouldBe` []
    it "deleteTask" $ do
        let task = Task{ description="task1", taskId=1, belongingUsers=[]}
        let (_, log) = evalTestFixture (TaskRepo.deleteTaskImpl task) fixture
        log `shouldBe` "1"
    it "updateDescription" $ do
        let task = Task{ description="task1", taskId=1, belongingUsers=[]}
        let (_, log) = evalTestFixture (TaskRepo.updateDescription task "task2") fixture
        let newTask = task {description = "task2"}
        log `shouldBe` show newTask
    it "updateTask" $ do
        let task = Task{ description="task1", taskId=1, belongingUsers=[]}
        let (_, log) = evalTestFixture (TaskRepo.updateTaskImpl task) fixture
        log `shouldBe` show task
    it "getTask" $ do
        let (result, _) = evalTestFixture (TaskRepo.getTaskImpl 1) fixture
        result `shouldBe` taskFromDb
