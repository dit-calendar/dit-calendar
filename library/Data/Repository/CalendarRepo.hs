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

updateCalendar :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) => CalendarEntry -> m ()
updateCalendar calendarEntry = update $ CalendarAcid.UpdateEntry calendarEntry

updateDescription:: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                  CalendarEntry -> String -> m ()
updateDescription calendarEntry newDescription =
    updateCalendar calendarEntry {CalendarEntry.description = newDescription}

deleteTaskFromCalendarEntry :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                Int -> CalendarEntry -> m ()
deleteTaskFromCalendarEntry taskId calendarEntry =
  updateCalendar calendarEntry {calendarTasks = delete taskId (calendarTasks calendarEntry)}

addTaskToCalendarEntry :: (HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
    TaskId -> CalendarEntry -> m ()
addTaskToCalendarEntry taskId calendarEntry =
    updateCalendar calendarEntry {calendarTasks = calendarTasks calendarEntry ++ [taskId]}
