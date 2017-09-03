{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserRepo
    ( deleteUser, updateName, addCalendarEntryToUser, addTaskToUser
    , deleteCalendarEntryFromUser, deleteTaskFromUser, getUser, createUser ) where

import Prelude hiding ( head )
import Happstack.Foundation              ( query, update, HasAcidState )
import Control.Monad.IO.Class
import Data.List                         ( delete )
import Data.Maybe                        ( fromJust )

import Data.Domain.User                  ( User(..) )
import Data.Domain.Types                 ( EntryId, TaskId, UserId )

import qualified Data.Repository.Acid.UserAcid       as UserAcid

createUser :: (HasAcidState m UserAcid.UserList, MonadIO m) => String -> m User
createUser name = let user = User { name = name
                    , userId = undefined
                    , calendarEntries = []
                    , belongingTasks = []
                    } in
        update $ UserAcid.NewUser user

deleteUser :: (MonadIO m, HasAcidState m UserAcid.UserList) =>
     User -> m ()
deleteUser user = update $ UserAcid.DeleteUser (Data.Domain.User.userId user)

updateUser :: (MonadIO m, HasAcidState m UserAcid.UserList) => User -> m ()
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

getUser :: (HasAcidState m UserAcid.UserList, MonadIO m) => UserId -> m User
getUser userId = do
        mUser <- query $ UserAcid.UserById userId
        return $ fromJust mUser