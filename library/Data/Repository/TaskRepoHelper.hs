module Data.Repository.TaskRepoHelper 
    ( deleteTask, createTask, addUserToTask, removeUserFromTask ) where

import Data.Domain.Task                      as Task
import Data.Domain.CalendarEntry             as CalendarEntry
import Control.Monad.IO.Class
import Data.List                ( delete )

import Data.Domain.Types                 ( UserId )
import Data.Repository.MonadDB.Calendar  ( MonadDBCalendar )
import Data.Repository.MonadDB.Task      ( MonadDBTask )
import Data.Repository.MonadDB.User      ( MonadDBUser )

import qualified Data.Repository.MonadDB.TaskRepo     as TaskMonad
import qualified Data.Repository.MonadDB.UserRepo     as UserMonad
import qualified Data.Repository.MonadDB.CalendarRepo as CalendarMonad
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Repository.TaskRepo             as TaskRepo
import qualified Data.Repository.CalendarRepo         as CalendarRepo


deleteTask :: (TaskMonad.MonadDBTaskRepo m, UserMonad.MonadDBUserHelper m, MonadIO m) =>
            Task -> m ()
deleteTask task = do
    TaskMonad.deleteTask task
    deleteTaskFromAllUsers task

createTask :: (TaskMonad.MonadDBTaskRepo m, UserMonad.MonadDBUserHelper m, CalendarMonad.MonadDBCalendarRepo m) =>
            CalendarEntry -> String -> m Task
createTask calendarEntry description = do
    mTask <- TaskMonad.createTask description
    CalendarMonad.addTaskToCalendarEntry calendarEntry (Task.taskId mTask)
    return mTask

deleteTaskFromAllUsers :: (UserMonad.MonadDBUserHelper m, MonadIO m) =>
                        Task -> m ()
deleteTaskFromAllUsers task =
    foldr (\ x ->
      (>>) (do
        user <- UserMonad.getUser x
        UserMonad.deleteTaskFromUser user x ))
    (return ()) $ Task.belongingUsers task

addUserToTask :: (MonadDBUser m, MonadDBTask m, MonadIO m) =>
                Task -> UserId -> m ()
addUserToTask task userId = do
    user <- UserRepo.getUser userId
    UserRepo.addTaskToUser user (taskId task)
    TaskRepo.updateTask task {belongingUsers = belongingUsers task ++ [userId]}

removeUserFromTask :: (TaskMonad.MonadDBTaskRepo m, UserMonad.MonadDBUserHelper m) =>
                    Task -> UserId -> m ()
removeUserFromTask task userId = do
    user <- UserMonad.getUser userId
    UserMonad.deleteTaskFromUser user (taskId task)
    TaskMonad.updateTask task {belongingUsers = delete userId (belongingUsers task)}
