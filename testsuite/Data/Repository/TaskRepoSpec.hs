{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Repository.TaskRepoSpec (spec) where

import Test.Hspec
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

import Control.Monad.IO.Class
import Control.Monad.Identity        ( Identity )
import Control.Monad.Writer.Class    ( tell )
import Control.Monad.Writer          ( execWriter )

import Happstack.Foundation          ( HasAcidState(..) )

import Data.Repository.MonadDB.Task     ( MonadDBTask )
import Data.Repository.Acid.TaskAcid        ( NewTask(..), DeleteTask(..), UpdateTask(..), TaskById(..) )
import Data.Domain.Task                     as Task

import qualified Data.Repository.TaskRepo          as TaskRepo


mkFixture "Fixture" [ts| MonadDBTask |]

taskFromDb = Task{ description="task1", taskId=1, belongingUsers=[]}

fixture = Fixture { _create = \(NewTask task) -> return task
                  , _delete = \(DeleteTask a) -> tell (show a)
                  , _update = \(UpdateTask a) -> tell (show a)
                  , _query  = \(TaskById a)   -> return (Just taskFromDb)}

instance MonadIO (TestFixtureT Fixture Char () Identity) where
    liftIO = undefined

spec = describe "CalendarRepo" $ do
    it "createTask" $ do
        let (result, _) = evalTestFixture (TaskRepo.createTask "task1") fixture
        Task.description result `shouldBe` "task1"
        Task.belongingUsers result `shouldBe` []
    it "deleteTask" $ do
        let task = Task{ description="task1", taskId=1, belongingUsers=[]}
        let (_, log) = evalTestFixture (TaskRepo.deleteTask task) fixture
        log `shouldBe` "1"
    it "updateDescription" $ do
        let task = Task{ description="task1", taskId=1, belongingUsers=[]}
        let (_, log) = evalTestFixture (TaskRepo.updateDescription task "task2") fixture
        let newTask = task {description = "task2"}
        log `shouldBe` show newTask
    it "updateTask" $ do
        let task = Task{ description="task1", taskId=1, belongingUsers=[]}
        let (_, log) = evalTestFixture (TaskRepo.updateTask task) fixture
        log `shouldBe` show task
    it "getTask" $ do
        let (result, _) = evalTestFixture (TaskRepo.getTask 1) fixture
        result `shouldBe` taskFromDb