{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserCalendarRepo where

import Prelude                  hiding ( head )
import Happstack.Foundation     ( query, update, HasAcidState )
import Data.Maybe               ( fromJust )
import Control.Monad.IO.Class

import Data.Repository.CalendarTaskRepo    as CalendarTaskRepo
import Data.Repository.Acid.UserAcid       as UserAcid
import Data.Repository.Acid.TaskAcid     ( TaskList )
import Data.Repository.Acid.CalendarAcid ( EntryList )
import Data.Domain.CalendarEntry           as CalendarEntry
import Data.Repository.CalendarRepo        as CalendarRepo
import Data.Repository.UserRepo            as UserRepo
import Data.Domain.User         ( User(..) )

deleteUser :: (MonadIO m, HasAcidState m EntryList, HasAcidState m UserList) =>
     User -> m ()
deleteUser user =
    let calendarToDelete = calendarEntries user in
        do
            CalendarRepo.deleteCalendar calendarToDelete
            update $ UserAcid.DeleteUser (Data.Domain.User.userId user)

removeCalendar :: (HasAcidState m EntryList, HasAcidState m UserAcid.UserList,
      HasAcidState m TaskList, MonadIO m) => CalendarEntry -> m ()
removeCalendar calendarEntry = let cEntryId = entryId calendarEntry in
    do
       mUser <- query (UserAcid.UserById (CalendarEntry.userId calendarEntry))
       UserRepo.deleteCalendarEntryFromUser (fromJust mUser) cEntryId
       CalendarTaskRepo.deleteCalendarsTasks calendarEntry
       deleteCalendar [cEntryId]