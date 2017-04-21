{-# LANGUAGE FlexibleContexts #-}

module Repository.UserRepo where

import Prelude                  hiding ( head )
import Happstack.Foundation     ( update, HasAcidState )
import Control.Monad.IO.Class

import Domain.User              ( User(..) )
import Domain.Types             ( UserId, EntryId )
import Repository.Acid.UserAcid as UserAcid

addCalendarEntryToUser :: (HasAcidState m UserAcid.UserList, MonadIO m) => 
    User -> EntryId -> m ()
addCalendarEntryToUser user entryId =
    let updatedUser = user {calendarEntrys = calendarEntrys user ++ [entryId]} in
        update $ UserAcid.UpdateUser updatedUser

