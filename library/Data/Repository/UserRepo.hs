{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserRepo  where

import Prelude hiding ( head )
import Happstack.Foundation              ( update, HasAcidState )
import Control.Monad.IO.Class
import Data.List                         ( delete )

import Data.Domain.User                  ( User(..) )
import Data.Domain.Types                 ( EntryId, TaskId )

import qualified Data.Repository.Acid.UserAcid       as UserAcid


deleteUser :: (MonadIO m, HasAcidState m UserAcid.UserList) =>
     User -> m ()
deleteUser user =
    let calendarToDelete = calendarEntries user in
        update $ UserAcid.DeleteUser (Data.Domain.User.userId user)

updateUser :: (MonadIO m, HasAcidState m UserAcid.UserList) =>
     User -> m ()
updateUser user = update $ UserAcid.UpdateUser user

updateName :: (MonadIO m, HasAcidState m UserAcid.UserList) =>
     User -> String -> m ()
updateName user newName = updateUser user {name = newName}

addCalendarEntryToUser :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
    User -> EntryId -> m ()
addCalendarEntryToUser user entryId =
    updateUser user {calendarEntries = calendarEntries user ++ [entryId]}

deleteCalendarEntryFromUser :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
                                   User -> EntryId -> m ()
deleteCalendarEntryFromUser user entryId =
    updateUser user {calendarEntries = delete entryId (calendarEntries user)}

addTaskToUser :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
    TaskId -> User -> m ()
addTaskToUser taskId user =
    updateUser user {belongingTasks = belongingTasks user ++ [taskId]}

deleteTaskFromUser :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
    TaskId -> User -> m ()
deleteTaskFromUser taskId user =
    updateUser user {belongingTasks = delete taskId (belongingTasks user)}