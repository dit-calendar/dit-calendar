module Data.Service.Task
    ( deleteTask, createTask, addUserToTask, removeUserFromTask ) where

import Data.Domain.Task                      as Task
import Data.Domain.CalendarEntry             as CalendarEntry
import Control.Monad.IO.Class
import Data.List                ( delete )

import Data.Domain.Types                  ( UserId )
import Data.Repository.Acid.MonadDB.Task  ( MonadDBTask )
import Data.Repository.Acid.MonadDB.User  ( MonadDBUser )

import qualified Data.Service.MonadDB.Task            as MonadDBTaskService
import Data.Service.MonadDB.Task                      ( MonadDBTaskService )
import qualified Data.Service.MonadDB.User            as MonadDBUserService
import Data.Service.MonadDB.User                      ( MonadDBUserService )
import qualified Data.Service.MonadDB.Calendar        as MonadDBCalendarService
import Data.Service.MonadDB.Calendar                  ( MonadDBCalendarService )
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Repository.TaskRepo             as TaskRepo
import qualified Data.Repository.CalendarRepo         as CalendarRepo


deleteTask :: (MonadDBTaskService m, MonadDBUserService m, MonadIO m) =>
            Task -> m ()
deleteTask task = do
    MonadDBTaskService.deleteTask task
    deleteTaskFromAllUsers task

createTask :: (MonadDBTaskService m, MonadDBUserService m, MonadDBCalendarService m) =>
            CalendarEntry -> String -> m Task
createTask calendarEntry description = do
    mTask <- MonadDBTaskService.createTask description
    MonadDBCalendarService.addTaskToCalendarEntry calendarEntry (Task.taskId mTask)
    return mTask

deleteTaskFromAllUsers :: (MonadDBUserService m, MonadIO m) =>
                        Task -> m ()
deleteTaskFromAllUsers task =
    foldr (\ x ->
      (>>) (do
        user <- MonadDBUserService.getUser x
        MonadDBUserService.deleteTaskFromUser user x ))
    (return ()) $ Task.belongingUsers task

addUserToTask :: (MonadDBUser m, MonadDBTask m, MonadIO m) =>
                Task -> UserId -> m ()
addUserToTask task userId = do
    user <- UserRepo.getUser userId
    UserRepo.addTaskToUser user (taskId task)
    TaskRepo.updateTask task {belongingUsers = belongingUsers task ++ [userId]}

removeUserFromTask :: (MonadDBTaskService m, MonadDBUserService m) =>
                    Task -> UserId -> m ()
removeUserFromTask task userId = do
    user <- MonadDBUserService.getUser userId
    MonadDBUserService.deleteTaskFromUser user (taskId task)
    MonadDBTaskService.updateTask task {belongingUsers = delete userId (belongingUsers task)}
