{-# LANGUAGE FlexibleContexts #-}
module Data.Repository.TaskRepoHelper where

import Happstack.Foundation     ( HasAcidState )
import Data.Domain.Task                      as Task
import Data.Domain.CalendarEntry             as CalendarEntry
import Control.Monad.IO.Class

import qualified Data.Repository.UserTaskRepo         as UserTaskRepo
import qualified Data.Repository.TaskRepo             as TaskRepo
import qualified Data.Repository.CalendarRepo         as CalendarRepo

import qualified Data.Repository.Acid.UserAcid        as UserAcid
import qualified Data.Repository.Acid.TaskAcid        as TaskAcid
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid

deleteTask :: (HasAcidState m TaskAcid.TaskList, HasAcidState m UserAcid.UserList, MonadIO m) =>
                   Task -> m ()
deleteTask task = do
    TaskRepo.deleteTask task
    UserTaskRepo.deleteTaskFromTasksUsers task

createTask :: (HasAcidState m CalendarAcid.EntryList,
      HasAcidState m TaskAcid.TaskList, MonadIO m) =>
    CalendarEntry -> String -> m Task
createTask calendarEntry description =
    do
        mTask <- TaskRepo.createTask calendarEntry description
        CalendarRepo.addTaskToCalendarEntry (Task.taskId mTask) calendarEntry
        return mTask