{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserTaskRepo where

import Happstack.Foundation       ( query, update, HasAcidState )
import Control.Monad.IO.Class
import Data.List                  ( delete )
import Data.Maybe                 ( fromJust )

import Data.Repository.Acid.TaskAcid          as TaskAcid
import Data.Repository.Acid.UserAcid          as UserAcid
import Data.Domain.Task                       as Task
import Data.Domain.User                       as User
import Data.Domain.Types          ( UserId, TaskId, EntryId )

addUserToTask :: (HasAcidState m TaskAcid.TaskList, HasAcidState m UserAcid.UserList, MonadIO m) =>
    Task -> UserId -> m ()
addUserToTask task userId =
    let updatedTask = task {belongingUsers = belongingUsers task ++ [userId]} in
        do
            mUser <- query (UserAcid.UserById userId)
            addTaskToUser (taskId task) (fromJust mUser)
            update $ TaskAcid.UpdateTask updatedTask

addTaskToUser :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
    TaskId -> User -> m ()
addTaskToUser taskId user =
    let updatedUser = user {belongingTasks = belongingTasks user ++ [taskId]} in
        update $ UserAcid.UpdateUser updatedUser

removeUserFromTask :: (HasAcidState m TaskAcid.TaskList, HasAcidState m UserAcid.UserList, MonadIO m) =>
                      Task -> UserId -> m ()
removeUserFromTask task userId =
    let updatedTask = task {belongingUsers = delete userId (belongingUsers task)} in
        do
            mUser <- query (UserAcid.UserById userId)
            deleteTaskFromUser (taskId task) (fromJust mUser)
            update $ TaskAcid.UpdateTask updatedTask

deleteTaskFromUser :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
    TaskId -> User -> m ()
deleteTaskFromUser taskId user =
    let updatedUser = user {belongingTasks = delete taskId (belongingTasks user)} in
        update $ UserAcid.UpdateUser updatedUser
