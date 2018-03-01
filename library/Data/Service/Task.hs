module Data.Service.Task
    ( deleteTask, createTask, addUserToTask, removeUserFromTask ) where

import Data.Domain.Task                      as Task
import Data.Domain.CalendarEntry             as CalendarEntry
import Control.Monad.IO.Class
import Data.List                ( delete )

import Data.Domain.Types                  ( UserId )

import qualified Data.Repository.TaskRepo            as MonadDBTaskRepo
import Data.Repository.TaskRepo                      ( MonadDBTaskRepo )
import qualified Data.Repository.UserRepo            as MonadDBUserRepo
import Data.Repository.UserRepo                      ( MonadDBUserRepo )
import qualified Data.Repository.CalendarRepo        as MonadDBCalendarRepo
import Data.Repository.CalendarRepo                  ( MonadDBCalendarRepo )


deleteTask :: (MonadDBTaskRepo m, MonadDBUserRepo m, MonadIO m) =>
            Task -> m ()
deleteTask task = do
    deleteTaskFromAllUsers task
    MonadDBTaskRepo.deleteTask task

createTask :: (MonadDBTaskRepo m, MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            CalendarEntry -> String -> m Task
createTask calendarEntry description = do
    mTask <- MonadDBTaskRepo.createTask description
    MonadDBCalendarRepo.addTaskToCalendarEntry calendarEntry (Task.taskId mTask)
    return mTask

deleteTaskFromAllUsers :: (MonadDBUserRepo m, MonadIO m) =>
                        Task -> m ()
deleteTaskFromAllUsers task =
    foldr (\ x ->
      (>>) (do
        user <- MonadDBUserRepo.getUser x
        MonadDBUserRepo.deleteTaskFromUser user x ))
    (return ()) $ Task.belongingUsers task

addUserToTask :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadIO m) =>
                Task -> UserId -> m ()
addUserToTask task userId = do
    user <- MonadDBUserRepo.getUser userId
    MonadDBUserRepo.addTaskToUser user (taskId task)
    MonadDBTaskRepo.updateTask task {belongingUsers = belongingUsers task ++ [userId]}

removeUserFromTask :: (MonadDBTaskRepo m, MonadDBUserRepo m) =>
                    Task -> UserId -> m ()
removeUserFromTask task userId = do
    user <- MonadDBUserRepo.getUser userId
    MonadDBUserRepo.deleteTaskFromUser user (taskId task)
    MonadDBTaskRepo.updateTask task {belongingUsers = delete userId (belongingUsers task)}
