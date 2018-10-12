{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.CalendarRepo
    ( newCalendarEntryImpl, deleteCalendarEntryImpl, updateDescription,
    deleteTaskFromCalendarEntryImpl, addTaskToCalendarEntryImpl, MonadDBCalendarRepo(..) ) where

import           Data.Time.Clock                    (UTCTime)
import qualified Data.List                          as List
import qualified Happstack.Foundation               as Foundation

import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.CalendarEntry          (CalendarEntry)
import           Data.Domain.Types                  (EntryId, TaskId, Description)
import           Data.Domain.User                   as User
import           Data.Domain.User                   (User)
import           Data.Repository.Acid.CalendarEntry (CalendarDAO (..))
import           Presentation.AcidHelper            (App)

import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid

instance CalendarDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

newCalendarEntryImpl :: CalendarDAO m => String -> Description -> User -> m CalendarEntry
newCalendarEntryImpl newDate description user = let entry = CalendarEntry {
                        description              = description
                        , entryId                = undefined
                        , CalendarEntry.userId   = User.userId user
                        , tasks          = []
                        , date                   = read newDate::UTCTime
                        } in
    create (CalendarEntryAcid.NewEntry entry)

deleteCalendarEntryImpl :: CalendarDAO m => EntryId -> m ()
deleteCalendarEntryImpl entryId = delete $ CalendarEntryAcid.DeleteEntry entryId

updateCalendar :: CalendarDAO m => CalendarEntry -> m ()
updateCalendar calendarEntry = update $ CalendarEntryAcid.UpdateEntry calendarEntry

updateDescription:: CalendarDAO m => CalendarEntry -> Description -> m ()
updateDescription calendarEntry newDescription =
    updateCalendar calendarEntry {CalendarEntry.description = newDescription}

deleteTaskFromCalendarEntryImpl :: CalendarDAO m =>
                            CalendarEntry -> Int -> m ()
deleteTaskFromCalendarEntryImpl calendarEntry taskId =
  updateCalendar calendarEntry {tasks = List.delete taskId (tasks calendarEntry)}

addTaskToCalendarEntryImpl :: CalendarDAO m =>
                        CalendarEntry -> TaskId -> m ()
addTaskToCalendarEntryImpl calendarEntry taskId =
    updateCalendar calendarEntry {tasks = tasks calendarEntry ++ [taskId]}

class (Monad m,  CalendarDAO App) => MonadDBCalendarRepo m where
    newCalendarEntry            :: String -> Description -> User -> m CalendarEntry
    deleteCalendarEntry         :: EntryId -> m ()
    deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m ()
    addTaskToCalendarEntry      :: CalendarEntry -> TaskId -> m ()

instance MonadDBCalendarRepo App where
    newCalendarEntry            = newCalendarEntryImpl
    deleteCalendarEntry         = deleteCalendarEntryImpl
    deleteTaskFromCalendarEntry = deleteTaskFromCalendarEntryImpl
    addTaskToCalendarEntry      = addTaskToCalendarEntryImpl
