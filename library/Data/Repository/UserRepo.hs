{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserRepo where

import Prelude                  hiding ( head )
import Happstack.Foundation     ( update, HasAcidState )
import Control.Monad.IO.Class
import Data.List                ( delete )

import Data.Domain.User              ( User(..) )
import Data.Domain.Task              ( Task(..) )
import Data.Domain.Types             ( UserId, EntryId )
import Data.Repository.Acid.UserAcid as UserAcid
import Data.Repository.Acid.TaskAcid as TaskAcid
import Data.Repository.Acid.CalendarAcid as CalendarAcid
import Data.Repository.CalendarRepo  ( deleteCalendar )

deleteUser :: (MonadIO m, HasAcidState m EntryList, HasAcidState m UserList) =>
     User -> m ()
deleteUser user =
    let calendarToDelete = calendarEntries user in
        do
            deleteCalendar calendarToDelete
            update $ UserAcid.DeleteUser (userId user)

updateUser :: (MonadIO m, HasAcidState m EntryList, HasAcidState m UserList) =>
     User -> String -> m ()
updateUser user newName =
    let updatedUser = user {name = newName} in
            update $ UserAcid.UpdateUser updatedUser

addUserToTask :: (HasAcidState m TaskAcid.TaskList, MonadIO m) =>
    Task -> UserId -> m ()
addUserToTask task userId =
    let updatedTask = task {belongingUsers = belongingUsers task ++ [userId]} in
        update $ TaskAcid.UpdateTask updatedTask

removeUserFromTask :: (HasAcidState m TaskAcid.TaskList, MonadIO m) =>
                      Task -> UserId -> m ()
removeUserFromTask task userId =
    let updatedTask = task {belongingUsers = delete userId (belongingUsers task)} in
        update $ TaskAcid.UpdateTask updatedTask
