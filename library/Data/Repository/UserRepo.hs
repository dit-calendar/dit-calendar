{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserRepo where

import Prelude                  hiding ( head )
import Happstack.Foundation     ( update, HasAcidState )
import Control.Monad.IO.Class

import Data.Domain.User              ( User(..) )
import Data.Domain.Types             ( UserId, EntryId )
import Data.Repository.Acid.UserAcid as UserAcid
import Data.Repository.Acid.CalendarAcid as CalendarAcid

deleteUser :: (MonadIO m, HasAcidState m EntryList, HasAcidState m UserList) =>
     User -> m ()
deleteUser user =
    let calendarToDelete = calendarEntrys user in
        do
            deleteCalendar calendarToDelete
            update $ UserAcid.DeleteUser (userId user)

deleteCalendar :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                   [EntryId] -> m ()
deleteCalendar = foldr (\ x -> (>>) (update $ CalendarAcid.DeleteEntry x))
        (return ())