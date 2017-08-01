{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserCalendarRepo where

import Prelude                  hiding ( head )
import Happstack.Foundation     ( update, HasAcidState )
import Control.Monad.IO.Class

import Data.Repository.Acid.UserAcid       as UserAcid
import Data.Repository.Acid.CalendarAcid ( EntryList )
import Data.Repository.CalendarRepo        as CalendarRepo
import Data.Domain.User         ( User(..) )

deleteUser :: (MonadIO m, HasAcidState m EntryList, HasAcidState m UserList) =>
     User -> m ()
deleteUser user =
    let calendarToDelete = calendarEntries user in
        do
            CalendarRepo.deleteCalendar calendarToDelete
            update $ UserAcid.DeleteUser (userId user)