{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.TaskRepo where

import Happstack.Foundation       ( update, HasAcidState )
import Control.Monad.IO.Class

import Data.Domain.CalendarEntry               as CalendarEntry
import Data.Domain.Task                        as Task

import qualified Data.Repository.Acid.TaskAcid          as TaskAcid
import qualified Data.Repository.Acid.UserAcid          as UserAcid
import qualified Data.Repository.Acid.CalendarAcid      as CalendarAcid



updateTask :: (HasAcidState m CalendarAcid.EntryList, HasAcidState m TaskAcid.TaskList, MonadIO m) =>
                  Task -> String -> m ()
updateTask task newDescription =
    let updatedTask = task {Task.description = newDescription} in
        update $ TaskAcid.UpdateTask updatedTask

deleteTask :: (HasAcidState m TaskAcid.TaskList, HasAcidState m UserAcid.UserList, MonadIO m) =>
                   Task -> m ()
deleteTask task = do
    update $ TaskAcid.DeleteTask $ taskId task

createTask :: (HasAcidState m CalendarAcid.EntryList,
      HasAcidState m TaskAcid.TaskList, MonadIO m) =>
    CalendarEntry -> String -> m Task
createTask calendarEntry description =
    do
        mTask <- update $ TaskAcid.NewTask description
        return mTask