{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.TaskRepo
    ( updateDescription, deleteTask, createTask, updateTask, getTask ) where

import Happstack.Foundation       ( query, update, HasAcidState )
import Control.Monad.IO.Class
import Data.Maybe                 ( fromJust )

import Data.Domain.CalendarEntry               as CalendarEntry
import Data.Domain.Task                        as Task
import Data.Domain.Types            ( TaskId )

import qualified Data.Repository.Acid.TaskAcid          as TaskAcid


updateTask :: (HasAcidState m TaskAcid.TaskList, MonadIO m) => Task -> m ()
updateTask task = update $ TaskAcid.UpdateTask task

updateDescription :: (HasAcidState m TaskAcid.TaskList, MonadIO m) =>
                  Task -> String -> m ()
updateDescription task newDescription = updateTask task {Task.description = newDescription}

deleteTask :: (HasAcidState m TaskAcid.TaskList, MonadIO m) =>
                   Task -> m ()
deleteTask task = update $ TaskAcid.DeleteTask $ taskId task

createTask :: (HasAcidState m TaskAcid.TaskList, MonadIO m) =>
              CalendarEntry -> String -> m Task
createTask calendarEntry description = update $ TaskAcid.NewTask description

getTask :: (HasAcidState m TaskAcid.TaskList, MonadIO m) => TaskId -> m Task
getTask taskId = do
        mTask <- query (TaskAcid.TaskById taskId)
        return $ fromJust mTask
