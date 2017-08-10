{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserTaskRepo where

import Happstack.Foundation       ( query, update, HasAcidState )
import Control.Monad.IO.Class
import Data.List                  ( delete )
import Data.Maybe                 ( fromJust )

import Data.Domain.Task                       as Task
import Data.Domain.User                       as User
import Data.Domain.Types          ( UserId, TaskId )

import qualified Data.Repository.Acid.TaskAcid          as TaskAcid
import qualified Data.Repository.Acid.UserAcid          as UserAcid
import qualified Data.Repository.UserRepo             as UserRepo


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

deleteTaskFromTasksUsers :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
    Task -> m ()
deleteTaskFromTasksUsers task =
    foldr (\ x ->
      (>>) (do
        mUser <- query (UserAcid.UserById x)
        UserRepo.deleteTaskFromUser x (fromJust mUser) ))
    (return ()) $ Task.belongingUsers task
