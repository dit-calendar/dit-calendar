{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Repository.TaskRepoSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Test.Hspec

import           Control.Monad.Identity       (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer         (execWriter)
import           Control.Monad.Writer.Class   (tell)
import           Data.Default                 (def)
import           Data.Maybe                   (fromJust)

import           AppContext                   (AppContext)
import           Data.Domain.Task             as Task
import           Data.Domain.User             as User
import           Data.Repository.Acid.Task    (DeleteTask (..), NewTask (..),
                                               TaskById (..), TaskDAO,
                                               UpdateTask (..))

import qualified Data.Repository.TaskRepo     as TaskRepo
import Test.HUnit.Base (assertEqual, assertFailure)


mkFixture "Fixture" [ts| TaskDAO, AppContext |]

taskFromDb = def{ Task.title="A", description=Just "task1", taskId=1, startTime=Nothing, endTime=Nothing, owner=1}
user = def { loginName="Foo", User.userId=10 }

fixture :: (Monad m, MonadWriter String m) => Fixture m
fixture = Fixture { _create = \(NewTask task) -> return task
                  , _delete = \(DeleteTask a) -> tell (show a)
                  , _update = \(UpdateTask a) -> tell (show a) >>= (\_ -> return $ Right a)
                  , _query  = \(TaskById a)   -> return (Just taskFromDb)
                  , _getCurrentUser = return $ Just user
                  }

instance MonadIO Identity where
    liftIO = undefined

spec = describe "TaskRepo" $ do
    it "createTask" $ do
        let (result, _) = evalTestFixture (TaskRepo.createTaskImpl taskFromDb) fixture
        result `shouldBe` taskFromDb
        Task.assignedTelegramLinks result `shouldBe` []
    it "deleteTask" $ do
        let task = def { Task.title="A", description=Just "task1", taskId=1}
        let (_, log) = evalTestFixture (TaskRepo.deleteTaskImpl task) fixture
        log `shouldBe` "1"
    it "updateTask" $ do
        let task = def { Task.title="A", description=Just "task1", taskId=1, startTime=Nothing, endTime=Nothing, owner=10}
        let (result, log) = evalTestFixture (TaskRepo.updateTaskImpl task) fixture
        assertEqual "update task with wrong task" log (show task)
        case result of
            Left _ -> assertFailure "updated calendar should be returned"
            Right r -> assertEqual "updated calendar should be returned" r task
    it "getTask" $ do
        let (result, _) = evalTestFixture (TaskRepo.findTaskByIdImpl 1) fixture
        result `shouldBe` Just taskFromDb
