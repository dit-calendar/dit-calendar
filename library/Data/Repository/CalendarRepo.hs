module Data.Repository.CalendarRepo
    ( createEntry, deleteCalendar, updateDescription,
    deleteTaskFromCalendarEntry, addTaskToCalendarEntry ) where

import Data.List                ( delete )

import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry
import Data.Domain.Types        ( EntryId, TaskId )

import qualified Data.Repository.MonadDB.Calendar     as DBRepo
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid


createEntry :: DBRepo.MonadDBCalendar m => String -> User -> m CalendarEntry
createEntry description user = let entry = CalendarEntry { 
                        description              = description
                        , entryId                = undefined
                        , CalendarEntry.userId   = User.userId user
                        , tasks          = []
                        } in
    DBRepo.create (CalendarAcid.NewEntry entry)

deleteCalendar :: DBRepo.MonadDBCalendar m => EntryId -> m ()
deleteCalendar entryId = DBRepo.delete $ CalendarAcid.DeleteEntry entryId

updateCalendar :: DBRepo.MonadDBCalendar m => CalendarEntry -> m ()
updateCalendar calendarEntry = DBRepo.update $ CalendarAcid.UpdateEntry calendarEntry

updateDescription:: DBRepo.MonadDBCalendar m => CalendarEntry -> String -> m ()
updateDescription calendarEntry newDescription =
    updateCalendar calendarEntry {CalendarEntry.description = newDescription}

deleteTaskFromCalendarEntry :: DBRepo.MonadDBCalendar m =>
                            CalendarEntry -> Int -> m ()
deleteTaskFromCalendarEntry calendarEntry taskId =
  updateCalendar calendarEntry {tasks = delete taskId (tasks calendarEntry)}

addTaskToCalendarEntry :: DBRepo.MonadDBCalendar m =>
                        CalendarEntry -> TaskId -> m ()
addTaskToCalendarEntry calendarEntry taskId =
    updateCalendar calendarEntry {tasks = tasks calendarEntry ++ [taskId]}
