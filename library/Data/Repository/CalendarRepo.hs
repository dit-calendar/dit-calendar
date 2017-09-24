{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.CalendarRepo
    ( createEntry, deleteCalendar, updateDescription,
    deleteTaskFromCalendarEntry, addTaskToCalendarEntry ) where

import Happstack.Foundation     ( HasAcidState )
import Control.Monad.IO.Class
import Data.List                ( delete )

import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry
import Data.Domain.Types        ( EntryId, TaskId )

import qualified Data.Repository.MonadDB.Calendar     as DBRepo
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid


createEntry :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                String -> User -> m CalendarEntry
createEntry description user = let entry = CalendarEntry { 
                        description              = description
                        , entryId                = undefined
                        , CalendarEntry.userId   = User.userId user
                        , calendarTasks          = []
                        } in
    DBRepo.create (CalendarAcid.NewEntry entry)

deleteCalendar :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                [EntryId] -> m ()
deleteCalendar = foldr (\ x -> (>>) (DBRepo.delete $ CalendarAcid.DeleteEntry x))
        (return ())

updateCalendar :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                CalendarEntry -> m ()
updateCalendar calendarEntry = DBRepo.update $ CalendarAcid.UpdateEntry calendarEntry

updateDescription:: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                CalendarEntry -> String -> m ()
updateDescription calendarEntry newDescription =
    updateCalendar calendarEntry {CalendarEntry.description = newDescription}

deleteTaskFromCalendarEntry :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                            Int -> CalendarEntry -> m ()
deleteTaskFromCalendarEntry taskId calendarEntry =
  updateCalendar calendarEntry {calendarTasks = delete taskId (calendarTasks calendarEntry)}

addTaskToCalendarEntry :: (DBRepo.MonadDBCalendar m, HasAcidState m CalendarAcid.EntryList, MonadIO m) =>
                        TaskId -> CalendarEntry -> m ()
addTaskToCalendarEntry taskId calendarEntry =
    updateCalendar calendarEntry {calendarTasks = calendarTasks calendarEntry ++ [taskId]}
