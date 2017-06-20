{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserRepo where

import Prelude                  hiding ( head )
import Happstack.Foundation     ( update, HasAcidState )
import Control.Monad.IO.Class

import Data.Domain.User              ( User(..) )
import Data.Domain.Types             ( UserId, EntryId )
import Data.Repository.Acid.UserAcid as UserAcid
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
