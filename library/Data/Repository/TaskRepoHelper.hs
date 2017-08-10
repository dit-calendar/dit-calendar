{-# LANGUAGE FlexibleContexts #-}
module Data.Repository.TaskRepoHelper 
    ( deleteTask, createTask, addUserToTask, removeUserFromTask ) where

import Happstack.Foundation     ( HasAcidState, query, update )
import Data.Domain.Task                      as Task
import Data.Domain.CalendarEntry             as CalendarEntry
import Control.Monad.IO.Class
import Data.Maybe               ( fromJust )
import Data.List                ( delete )

import Data.Domain.Types        ( UserId )
import qualified Data.Repository.UserRepo             as UserRepo
import qualified Data.Repository.TaskRepo             as TaskRepo
import qualified Data.Repository.CalendarRepo         as CalendarRepo

import qualified Data.Repository.Acid.UserAcid        as UserAcid
import qualified Data.Repository.Acid.TaskAcid        as TaskAcid
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid

deleteTask :: (HasAcidState m TaskAcid.TaskList, HasAcidState m UserAcid.UserList, MonadIO m) =>
                   Task -> m ()
deleteTask task = do
    TaskRepo.deleteTask task
    deleteTaskFromAllUsers task

createTask :: (HasAcidState m CalendarAcid.EntryList,
      HasAcidState m TaskAcid.TaskList, MonadIO m) =>
    CalendarEntry -> String -> m Task
createTask calendarEntry description =
    do
        mTask <- TaskRepo.createTask calendarEntry description
        CalendarRepo.addTaskToCalendarEntry (Task.taskId mTask) calendarEntry
        return mTask

deleteTaskFromAllUsers :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
    Task -> m ()
deleteTaskFromAllUsers task =
    foldr (\ x ->
      (>>) (do
        mUser <- query (UserAcid.UserById x)
        UserRepo.deleteTaskFromUser x (fromJust mUser) ))
    (return ()) $ Task.belongingUsers task

addUserToTask :: (HasAcidState m TaskAcid.TaskList, HasAcidState m UserAcid.UserList, MonadIO m) =>
    Task -> UserId -> m ()
addUserToTask task userId =
    let updatedTask = task {belongingUsers = belongingUsers task ++ [userId]} in
        do
            mUser <- query (UserAcid.UserById userId)
            UserRepo.addTaskToUser (taskId task) (fromJust mUser)
            update $ TaskAcid.UpdateTask updatedTask

removeUserFromTask :: (HasAcidState m TaskAcid.TaskList, HasAcidState m UserAcid.UserList, MonadIO m) =>
                      Task -> UserId -> m ()
removeUserFromTask task userId =
    let updatedTask = task {belongingUsers = delete userId (belongingUsers task)} in
        do
            mUser <- query (UserAcid.UserById userId)
            UserRepo.deleteTaskFromUser (taskId task) (fromJust mUser)
            update $ TaskAcid.UpdateTask updatedTask
