module Data.Service.Task
    ( deleteTask, createTask, addUserToTask, removeUserFromTask ) where

import Data.Domain.Task                      as Task
import Data.Domain.CalendarEntry             as CalendarEntry
import Control.Monad.IO.Class
import Data.List                ( delete )

import Data.Domain.Types                  ( UserId )
import Data.Repository.Acid.Task          ( MonadDBTask )
import Data.Repository.Acid.User          ( MonadDBUser )

import qualified Data.Repository.MonadDB.Task            as MonadDBTaskRepo
import Data.Repository.MonadDB.Task                      ( MonadDBTaskRepo )
import qualified Data.Repository.MonadDB.User            as MonadDBUserRepo
import Data.Repository.MonadDB.User                      ( MonadDBUserRepo )
import qualified Data.Repository.MonadDB.Calendar        as MonadDBCalendarRepo
import Data.Repository.MonadDB.Calendar                  ( MonadDBCalendarRepo )
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Repository.TaskRepo             as TaskRepo
import qualified Data.Repository.CalendarRepo         as CalendarRepo


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

addUserToTask :: (MonadDBUser m, MonadDBTask m, MonadIO m) =>
                Task -> UserId -> m ()
addUserToTask task userId = do
    user <- UserRepo.getUser userId
    UserRepo.addTaskToUser user (taskId task)
    TaskRepo.updateTask task {belongingUsers = belongingUsers task ++ [userId]}

removeUserFromTask :: (MonadDBTaskRepo m, MonadDBUserRepo m) =>
                    Task -> UserId -> m ()
removeUserFromTask task userId = do
    user <- MonadDBUserRepo.getUser userId
    MonadDBUserRepo.deleteTaskFromUser user (taskId task)
    MonadDBTaskRepo.updateTask task {belongingUsers = delete userId (belongingUsers task)}
