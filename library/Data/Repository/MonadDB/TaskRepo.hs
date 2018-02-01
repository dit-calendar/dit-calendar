{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Repository.MonadDB.TaskRepo where

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.Task           ( Task(..) )
import Data.Domain.Types          ( TaskId )

import qualified Data.Repository.MonadDB.Task     as DBRepo
import qualified Data.Repository.TaskRepo   as TaskRepo

class Monad m => MonadDBTaskRepo m where
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