{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.CalendarRepo where

import Happstack.Foundation     ( update, HasAcidState )
import Control.Monad.IO.Class
import Data.List                ( delete )

import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry
import Data.Domain.Types        ( EntryId, TaskId )

import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid


createEntry :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                String -> User -> m CalendarEntry
createEntry description user =
    update (CalendarAcid.NewEntry description $ User.userId user)

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

addTaskToCalendarEntry :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
    TaskId -> CalendarEntry -> m ()
addTaskToCalendarEntry taskId calendarEntry =
    let updatedCalendarEntry = calendarEntry {calendarTasks = calendarTasks calendarEntry ++ [taskId]} in
        update $ CalendarAcid.UpdateEntry updatedCalendarEntry
