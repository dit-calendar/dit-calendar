{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.CalendarRepo
    ( createCalendarEntryImpl
    , deleteCalendarEntryByIdImpl
    , updateCalendarImpl
    , deleteTaskFromCalendarEntryImpl
    , addTaskToCalendarEntryImpl
    , findCalendarByIdImpl
    , findAllCalendarEntriesImpl
    , MonadDBCalendarRepo(..)
    ) where

import           Control.Monad.IO.Class             (MonadIO)
import           Data.Default                       (def)
import qualified Data.List                          as List
import           Data.Maybe                         (fromJust)
import           Data.Time.Clock                    (UTCTime)

import qualified Happstack.Foundation               as Foundation

import           AppContext                         (App, AppContext)
import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.CalendarEntry          (CalendarEntry)
import           Data.Domain.Types                  (Description, EitherResult,
                                                     EntryId, TaskId)
import           Data.Domain.User                   as User
import           Data.Domain.User                   (User)
import           Data.Repository.Acid.CalendarEntry (CalendarDAO (..))
import           Data.Repository.PermissionControl  (executeUnderUserPermission)
import           Server.AcidInitializer

import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid

instance CalendarDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query = Foundation.query
    findList = Foundation.query

findAllCalendarEntriesImpl :: (CalendarDAO m, MonadIO m) => User -> m [CalendarEntry]
findAllCalendarEntriesImpl = findList . CalendarEntryAcid.AllEntriesForUser

createCalendarEntryImpl :: CalendarDAO m => CalendarEntry-> m CalendarEntry
createCalendarEntryImpl = create . CalendarEntryAcid.NewEntry

deleteCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> m ()
deleteCalendarEntryImpl = delete . CalendarEntryAcid.DeleteEntry . entryId

deleteCalendarEntryByIdImpl :: CalendarDAO m => EntryId -> m ()
deleteCalendarEntryByIdImpl = delete . CalendarEntryAcid.DeleteEntry

updateCalendarImpl :: (CalendarDAO m, AppContext m ) => CalendarEntry -> m (EitherResult CalendarEntry)
updateCalendarImpl calendarEntry =
    executeUnderUserPermission calendarEntry (update  $ CalendarEntryAcid.UpdateEntry calendarEntry)

updateTaskInCalendar :: CalendarDAO m => CalendarEntry -> m (EitherResult CalendarEntry)
updateTaskInCalendar = update . CalendarEntryAcid.UpdateEntry

findCalendarByIdImpl :: (CalendarDAO m, MonadIO m) => EntryId -> m (Maybe CalendarEntry)
findCalendarByIdImpl = query . CalendarEntryAcid.EntryById

deleteTaskFromCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> Int -> m (EitherResult CalendarEntry)
deleteTaskFromCalendarEntryImpl calendarEntry taskId =
    updateTaskInCalendar calendarEntry {tasks = List.delete taskId (tasks calendarEntry)}

addTaskToCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> TaskId -> m (EitherResult CalendarEntry)
addTaskToCalendarEntryImpl calendarEntry taskId = updateTaskInCalendar calendarEntry {tasks = taskId : tasks calendarEntry}

class Monad m => MonadDBCalendarRepo m where
    createCalendarEntry :: CalendarEntry -> m CalendarEntry
    findCalendarById :: EntryId -> m (Maybe CalendarEntry)
    findAllCalendarEntries :: User -> m [CalendarEntry]
    updateCalendar :: CalendarEntry -> m (EitherResult CalendarEntry)
    deleteCalendarEntryById :: EntryId -> m ()
    deleteCalendarEntry :: CalendarEntry -> m ()
    deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m (EitherResult CalendarEntry)
    addTaskToCalendarEntry :: CalendarEntry -> TaskId -> m (EitherResult CalendarEntry)

instance CalendarDAO App => MonadDBCalendarRepo App where
    createCalendarEntry = createCalendarEntryImpl
    findCalendarById = findCalendarByIdImpl
    findAllCalendarEntries = findAllCalendarEntriesImpl
    updateCalendar = updateCalendarImpl
    deleteCalendarEntryById = deleteCalendarEntryByIdImpl
    deleteCalendarEntry = deleteCalendarEntryImpl
    deleteTaskFromCalendarEntry = deleteTaskFromCalendarEntryImpl
    addTaskToCalendarEntry = addTaskToCalendarEntryImpl
