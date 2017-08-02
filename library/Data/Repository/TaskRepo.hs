{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.TaskRepo where

import Happstack.Foundation       ( update, HasAcidState )
import Control.Monad.IO.Class

import Data.Repository.UserTaskRepo           as UserTaskRepo
import Data.Repository.Acid.TaskAcid          as TaskAcid
import Data.Repository.Acid.UserAcid          as UserAcid
import Data.Repository.Acid.CalendarAcid      as CalendarAcid
import Data.Domain.CalendarEntry              as CalendarEntry
import Data.Domain.Task                       as Task


updateTask :: (HasAcidState m CalendarAcid.EntryList, HasAcidState m TaskList, MonadIO m) =>
                  Task -> String -> m ()
updateTask task newDescription =
    let updatedTask = task {Task.description = newDescription} in
        update $ TaskAcid.UpdateTask updatedTask

deleteTask :: (HasAcidState m TaskAcid.TaskList, HasAcidState m UserAcid.UserList, MonadIO m) =>
                   Task -> m ()
deleteTask task = do
    update $ TaskAcid.DeleteTask $ taskId task
    UserTaskRepo.deleteTaskFromTasksUsers task