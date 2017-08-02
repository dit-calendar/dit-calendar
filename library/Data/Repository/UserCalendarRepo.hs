{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.UserCalendarRepo where

import Prelude                  hiding ( head )
import Happstack.Foundation     ( query, update, HasAcidState )
import Data.Maybe               ( fromJust )
import Control.Monad.IO.Class

import Data.Domain.User         ( User(..) )
import Data.Domain.CalendarEntry           as CalendarEntry

import qualified Data.Repository.CalendarTaskRepo    as CalendarTaskRepo
import qualified Data.Repository.Acid.UserAcid       as UserAcid
import qualified Data.Repository.Acid.TaskAcid       as TaskAcid
import qualified Data.Repository.Acid.CalendarAcid   as CalendarAcid
import qualified Data.Repository.CalendarRepo        as CalendarRepo
import qualified Data.Repository.UserRepo            as UserRepo


deleteUser :: (MonadIO m, HasAcidState m CalendarAcid.EntryList, HasAcidState m UserAcid.UserList) =>
     User -> m ()
deleteUser user =
    let calendarToDelete = calendarEntries user in
        do
            CalendarRepo.deleteCalendar calendarToDelete
            update $ UserAcid.DeleteUser (Data.Domain.User.userId user)

removeCalendar :: (HasAcidState m CalendarAcid.EntryList, HasAcidState m UserAcid.UserList,
      HasAcidState m TaskAcid.TaskList, MonadIO m) => CalendarEntry -> m ()
removeCalendar calendarEntry = let cEntryId = entryId calendarEntry in
    do
       mUser <- query (UserAcid.UserById (CalendarEntry.userId calendarEntry))
       UserRepo.deleteCalendarEntryFromUser (fromJust mUser) cEntryId
       CalendarTaskRepo.deleteCalendarsTasks calendarEntry
       CalendarRepo.deleteCalendar [cEntryId]