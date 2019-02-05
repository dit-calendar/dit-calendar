{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.CalendarRepo
    ( createCalendarEntryImpl
    , deleteCalendarEntryImpl
    , updateCalendarImpl
    , deleteTaskFromCalendarEntryImpl
    , addTaskToCalendarEntryImpl
    , findCalendarByIdImpl
    , MonadDBCalendarRepo(..)
    ) where

import           Control.Monad.IO.Class             (MonadIO)
import           Data.Default                       (def)
import qualified Data.List                          as List
import           Data.Maybe                         (fromJust)
import           Data.Time.Clock                    (UTCTime)

import qualified Happstack.Foundation               as Foundation

import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.CalendarEntry          (CalendarEntry)
import           Data.Domain.Types                  (Description, EntryId,
                                                     TaskId)
import           Data.Domain.User                   as User
import           Data.Domain.User                   (User)
import           Data.Repository.Acid.CalendarEntry (CalendarDAO (..))
import           Data.Repository.Acid.Types         (UpdateReturn)
import           AcidHelper            (App)

import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid

instance CalendarDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query = Foundation.query

createCalendarEntryImpl :: CalendarDAO m => CalendarEntry-> m CalendarEntry
createCalendarEntryImpl entry = create (CalendarEntryAcid.NewEntry entry)

deleteCalendarEntryImpl :: CalendarDAO m => EntryId -> m ()
deleteCalendarEntryImpl entryId = delete $ CalendarEntryAcid.DeleteEntry entryId

updateCalendarImpl :: CalendarDAO m => CalendarEntry -> m (UpdateReturn CalendarEntry)
updateCalendarImpl calendarEntry = update $ CalendarEntryAcid.UpdateEntry calendarEntry

findCalendarByIdImpl :: (CalendarDAO m, MonadIO m) => EntryId -> m (Maybe CalendarEntry)
findCalendarByIdImpl entryId = query $ CalendarEntryAcid.EntryById entryId

deleteTaskFromCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> Int -> m (UpdateReturn CalendarEntry)
deleteTaskFromCalendarEntryImpl calendarEntry taskId =
    updateCalendarImpl calendarEntry {tasks = List.delete taskId (tasks calendarEntry)}

addTaskToCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> TaskId -> m (UpdateReturn CalendarEntry)
addTaskToCalendarEntryImpl calendarEntry taskId = updateCalendarImpl calendarEntry {tasks = tasks calendarEntry ++ [taskId]}

class (Monad m, CalendarDAO App) =>
      MonadDBCalendarRepo m
    where
    createCalendarEntry :: CalendarEntry -> m CalendarEntry
    findCalendarById :: EntryId -> m (Maybe CalendarEntry)
    updateCalendar :: CalendarEntry -> m (UpdateReturn CalendarEntry)
    deleteCalendarEntry :: EntryId -> m ()
    deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m (UpdateReturn CalendarEntry)
    addTaskToCalendarEntry :: CalendarEntry -> TaskId -> m (UpdateReturn CalendarEntry)

instance MonadDBCalendarRepo App where
    createCalendarEntry = createCalendarEntryImpl
    findCalendarById = findCalendarByIdImpl
    updateCalendar = updateCalendarImpl
    deleteCalendarEntry = deleteCalendarEntryImpl
    deleteTaskFromCalendarEntry = deleteTaskFromCalendarEntryImpl
    addTaskToCalendarEntry = addTaskToCalendarEntryImpl
