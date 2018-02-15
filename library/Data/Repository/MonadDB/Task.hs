{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances #-}

module Data.Repository.MonadDB.Task where

import Control.Monad.IO.Class

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.Task           ( Task(..) )
import Data.Domain.Types          ( TaskId )
import Data.Repository.Acid.CalendarEntry        ( MonadDBCalendar )
import Data.Repository.Acid.Task                 ( MonadDBTask )
import Data.Repository.Acid.User                 ( MonadDBUser )

import qualified Data.Repository.TaskRepo   as TaskRepo

class Monad m => MonadDBTaskRepo m where
    updateTask        :: Task   -> m ()
    deleteTask        :: Task   -> m ()
    createTask        :: String -> m Task
    getTask           :: TaskId -> m Task

instance (MonadDBUser CtrlV', MonadDBTask CtrlV', MonadDBCalendar CtrlV')
        => MonadDBTaskRepo CtrlV' where
    updateTask        = TaskRepo.updateTask
    deleteTask        = TaskRepo.deleteTask
    createTask        = TaskRepo.createTask
    getTask           = TaskRepo.getTask