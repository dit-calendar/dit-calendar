{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Repository.MonadDB.TaskRepo where

import Control.Monad.IO.Class

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.Task           ( Task(..) )
import Data.Domain.Types          ( TaskId )
import Data.Repository.MonadDB.Calendar     ( MonadDBCalendar )
import Data.Repository.MonadDB.Task         ( MonadDBTask )
import Data.Repository.MonadDB.User         ( MonadDBUser )

import qualified Data.Repository.TaskRepo   as TaskRepo

class (MonadDBUser m, MonadDBTask m, MonadIO m, Monad m) => MonadDBTaskRepo m where
  updateTask        :: Task   -> m ()
  updateDescription :: Task   -> String -> m ()
  deleteTask        :: Task   -> m ()
  createTask        :: String -> m Task
  getTask           :: TaskId -> m Task

instance MonadDBTaskRepo CtrlV' where
    updateTask        = TaskRepo.updateTask
    updateDescription = TaskRepo.updateDescription
    deleteTask        = TaskRepo.deleteTask
    createTask        = TaskRepo.createTask
    getTask           = TaskRepo.getTask