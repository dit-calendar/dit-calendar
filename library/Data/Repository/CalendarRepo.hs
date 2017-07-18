{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.CalendarRepo where

import Happstack.Foundation     ( query, update, HasAcidState )
import Control.Monad.IO.Class
import Data.List                ( delete )
import Data.Maybe               ( fromJust )

import Data.Domain.User                     as User
import Data.Domain.CalendarEntry            as CalendarEntry
import Data.Domain.Types        ( UserId, EntryId )
import Data.Repository.TaskRepo             as TaskRepo
import Data.Repository.Acid.UserAcid        as UserAcid
import Data.Repository.Acid.TaskAcid        as TaskAcid
import Data.Repository.Acid.CalendarAcid    as CalendarAcid


createEntry :: (HasAcidState m EntryList, HasAcidState m UserAcid.UserList,
            MonadIO m) => String -> User -> m CalendarEntry
createEntry description user = do
    calendarEntry <- update (CalendarAcid.NewEntry description $ User.userId user)
    addCalendarEntryToUser user $ CalendarEntry.entryId calendarEntry
    return calendarEntry

addCalendarEntryToUser :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
    User -> EntryId -> m ()
addCalendarEntryToUser user entryId =
    let updatedUser = user {calendarEntries = calendarEntries user ++ [entryId]} in
        update $ UserAcid.UpdateUser updatedUser

removeCalendar :: (HasAcidState m EntryList, HasAcidState m UserAcid.UserList,
      HasAcidState m TaskList, MonadIO m) => CalendarEntry -> m ()
removeCalendar calendarEntry = let cEntryId = entryId calendarEntry in
    do
       mUser <- query (UserAcid.UserById (CalendarEntry.userId calendarEntry))
       deleteCalendarEntryFromUser (fromJust mUser) cEntryId
       TaskRepo.deleteTask $ CalendarEntry.calendarTasks calendarEntry
       deleteCalendar [cEntryId]

deleteCalendarEntryFromUser :: (HasAcidState m UserAcid.UserList, MonadIO m) =>
                                   User -> EntryId -> m ()
deleteCalendarEntryFromUser user entryId =
    let updatedUser = user {calendarEntries = delete entryId (calendarEntries user)} in
        update $ UserAcid.UpdateUser updatedUser

deleteCalendar :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                   [EntryId] -> m ()
deleteCalendar = foldr (\ x -> (>>) (update $ CalendarAcid.DeleteEntry x))
        (return ())

updateCalendar :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                  CalendarEntry -> String -> m ()
updateCalendar calendarEntry newDescription =
    let updatedEntry = calendarEntry {CalendarEntry.description = newDescription} in
            update $ CalendarAcid.UpdateEntry updatedEntry

deleteTaskFromCalendarEntry :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                Int -> CalendarEntry -> m ()
deleteTaskFromCalendarEntry taskId calendarEntry =
  let updatedCalendarEntry = calendarEntry {calendarTasks = delete taskId (calendarTasks calendarEntry)} in
        update $ CalendarAcid.UpdateEntry updatedCalendarEntry
