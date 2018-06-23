{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.CalendarRepo
    ( newCalendarEntryImpl, deleteCalendarEntryImpl, updateDescription,
    deleteTaskFromCalendarEntryImpl, addTaskToCalendarEntryImpl, MonadDBCalendarRepo(..) ) where

import qualified Data.List                          as List
import qualified Happstack.Foundation               as Foundation

import           Controller.AcidHelper              (CtrlV')
import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.CalendarEntry          (CalendarEntry)
import           Data.Domain.Types                  (EntryId, TaskId)
import           Data.Domain.User                   as User
import           Data.Domain.User                   (User)

import           Data.Repository.Acid.CalendarEntry (MonadDBCalendar (..))
import           Data.Repository.Acid.Task          (MonadDBTask)
import           Data.Repository.Acid.User          (MonadDBUser)
import           Data.Time.Clock                    (UTCTime)

import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid

instance MonadDBCalendar CtrlV' where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

newCalendarEntryImpl :: MonadDBCalendar m => String -> String -> User -> m CalendarEntry
newCalendarEntryImpl newDate description user = let entry = CalendarEntry {
                        description              = description
                        , entryId                = undefined
                        , CalendarEntry.userId   = User.userId user
                        , tasks          = []
                        , date                   = read newDate::UTCTime
                        } in
    create (CalendarEntryAcid.NewEntry entry)

deleteCalendarEntryImpl :: MonadDBCalendar m => EntryId -> m ()
deleteCalendarEntryImpl entryId = delete $ CalendarEntryAcid.DeleteEntry entryId

updateCalendar :: MonadDBCalendar m => CalendarEntry -> m ()
updateCalendar calendarEntry = update $ CalendarEntryAcid.UpdateEntry calendarEntry

updateDescription:: MonadDBCalendar m => CalendarEntry -> String -> m ()
updateDescription calendarEntry newDescription =
    updateCalendar calendarEntry {CalendarEntry.description = newDescription}

deleteTaskFromCalendarEntryImpl :: MonadDBCalendar m =>
                            CalendarEntry -> Int -> m ()
deleteTaskFromCalendarEntryImpl calendarEntry taskId =
  updateCalendar calendarEntry {tasks = List.delete taskId (tasks calendarEntry)}

addTaskToCalendarEntryImpl :: MonadDBCalendar m =>
                        CalendarEntry -> TaskId -> m ()
addTaskToCalendarEntryImpl calendarEntry taskId =
    updateCalendar calendarEntry {tasks = tasks calendarEntry ++ [taskId]}

class Monad m => MonadDBCalendarRepo m where
    newCalendarEntry            :: String -> String -> User -> m CalendarEntry
    deleteCalendarEntry         :: EntryId -> m ()
    deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m ()
    addTaskToCalendarEntry      :: CalendarEntry -> TaskId -> m ()

instance (MonadDBUser CtrlV', MonadDBCalendar CtrlV', MonadDBTask CtrlV')
        => MonadDBCalendarRepo CtrlV' where
    newCalendarEntry            = newCalendarEntryImpl
    deleteCalendarEntry         = deleteCalendarEntryImpl
    deleteTaskFromCalendarEntry = deleteTaskFromCalendarEntryImpl
    addTaskToCalendarEntry      = addTaskToCalendarEntryImpl
