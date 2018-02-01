{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Repository.CalendarRepo
    ( MonadDBCalendar(..), newCalendarEntry, deleteCalendarEntry, updateDescription,
    deleteTaskFromCalendarEntry, addTaskToCalendarEntry ) where

import qualified Data.List                  as List
import qualified Happstack.Foundation       as Foundation

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.Types          ( EntryId, TaskId )
import Data.Domain.User                      as User
import Data.Domain.CalendarEntry             as CalendarEntry

import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid


class Monad m => MonadDBCalendar m where
  create :: CalendarAcid.NewEntry -> m CalendarEntry
  update :: CalendarAcid.UpdateEntry -> m ()
  delete :: CalendarAcid.DeleteEntry -> m ()
  query  :: CalendarAcid.EntryById -> m (Maybe CalendarEntry)

instance MonadDBCalendar CtrlV' where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query


newCalendarEntry :: MonadDBCalendar m => String -> User -> m CalendarEntry
newCalendarEntry description user = let entry = CalendarEntry { 
                        description              = description
                        , entryId                = undefined
                        , CalendarEntry.userId   = User.userId user
                        , tasks          = []
                        } in
    create (CalendarAcid.NewEntry entry)

deleteCalendarEntry :: MonadDBCalendar m => EntryId -> m ()
deleteCalendarEntry entryId = delete $ CalendarAcid.DeleteEntry entryId

updateCalendar :: MonadDBCalendar m => CalendarEntry -> m ()
updateCalendar calendarEntry = update $ CalendarAcid.UpdateEntry calendarEntry

updateDescription:: MonadDBCalendar m => CalendarEntry -> String -> m ()
updateDescription calendarEntry newDescription =
    updateCalendar calendarEntry {CalendarEntry.description = newDescription}

deleteTaskFromCalendarEntry :: MonadDBCalendar m =>
                            CalendarEntry -> Int -> m ()
deleteTaskFromCalendarEntry calendarEntry taskId =
  updateCalendar calendarEntry {tasks = List.delete taskId (tasks calendarEntry)}

addTaskToCalendarEntry :: MonadDBCalendar m =>
                        CalendarEntry -> TaskId -> m ()
addTaskToCalendarEntry calendarEntry taskId =
    updateCalendar calendarEntry {tasks = tasks calendarEntry ++ [taskId]}
