{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.CalendarRepo
    ( createEntry, deleteCalendar, updateDescription,
    deleteTaskFromCalendarEntry, addTaskToCalendarEntry ) where

import Happstack.Foundation     ( HasAcidState )
import Data.List                ( delete )

import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry
import Data.Domain.Types        ( EntryId, TaskId )

import qualified Data.Repository.MonadDB.Calendar     as DBRepo
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid


createEntry :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList) =>
                String -> User -> m CalendarEntry
createEntry description user = let entry = CalendarEntry { 
                        description              = description
                        , entryId                = undefined
                        , CalendarEntry.userId   = User.userId user
                        , tasks          = []
                        } in
    DBRepo.create (CalendarAcid.NewEntry entry)

deleteCalendar :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList) =>
                [EntryId] -> m ()
deleteCalendar = foldr (\ x -> (>>) (DBRepo.delete $ CalendarAcid.DeleteEntry x))
        (return ())

updateCalendar :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList) =>
                CalendarEntry -> m ()
updateCalendar calendarEntry = DBRepo.update $ CalendarAcid.UpdateEntry calendarEntry

updateDescription:: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList) =>
                CalendarEntry -> String -> m ()
updateDescription calendarEntry newDescription =
    updateCalendar calendarEntry {CalendarEntry.description = newDescription}

deleteTaskFromCalendarEntry :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList) =>
                            Int -> CalendarEntry -> m ()
deleteTaskFromCalendarEntry taskId calendarEntry =
  updateCalendar calendarEntry {tasks = delete taskId (tasks calendarEntry)}

addTaskToCalendarEntry :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList) =>
                        TaskId -> CalendarEntry -> m ()
addTaskToCalendarEntry taskId calendarEntry =
    updateCalendar calendarEntry {tasks = tasks calendarEntry ++ [taskId]}
