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
    , findAllCalendarEntriesImpl
    , MonadDBCalendarRepo(..)
    ) where

import           Control.Monad.IO.Class             (MonadIO)
import           Data.Default                       (def)
import qualified Data.List                          as List
import           Data.Maybe                         (fromJust)
import           Data.Time.Clock                    (UTCTime)

import qualified Happstack.Foundation               as Foundation

import           AcidHelper                         (App)
import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.CalendarEntry          (CalendarEntry)
import           Data.Domain.Types                  (Description,
                                                     EitherResponse, EntryId,
                                                     TaskId)
import           Data.Domain.User                   as User
import           Data.Domain.User                   (User)
import           Data.Repository.Acid.CalendarEntry (CalendarDAO (..))

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

deleteCalendarEntryImpl :: CalendarDAO m => EntryId -> m ()
deleteCalendarEntryImpl = delete . CalendarEntryAcid.DeleteEntry

updateCalendarImpl :: CalendarDAO m => CalendarEntry -> m (EitherResponse CalendarEntry)
updateCalendarImpl = update . CalendarEntryAcid.UpdateEntry

findCalendarByIdImpl :: (CalendarDAO m, MonadIO m) => EntryId -> m (Maybe CalendarEntry)
findCalendarByIdImpl = query . CalendarEntryAcid.EntryById

deleteTaskFromCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> Int -> m (EitherResponse CalendarEntry)
deleteTaskFromCalendarEntryImpl calendarEntry taskId =
    updateCalendarImpl calendarEntry {tasks = List.delete taskId (tasks calendarEntry)}

addTaskToCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> TaskId -> m (EitherResponse CalendarEntry)
addTaskToCalendarEntryImpl calendarEntry taskId = updateCalendarImpl calendarEntry {tasks = taskId : tasks calendarEntry}

class (Monad m, CalendarDAO App) =>
      MonadDBCalendarRepo m
    where
    createCalendarEntry :: CalendarEntry -> m CalendarEntry
    findCalendarById :: EntryId -> m (Maybe CalendarEntry)
    findAllCalendarEntries :: User -> m [CalendarEntry]
    updateCalendar :: CalendarEntry -> m (EitherResponse CalendarEntry)
    deleteCalendarEntry :: EntryId -> m ()
    deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m (EitherResponse CalendarEntry)
    addTaskToCalendarEntry :: CalendarEntry -> TaskId -> m (EitherResponse CalendarEntry)

instance MonadDBCalendarRepo App where
    createCalendarEntry = createCalendarEntryImpl
    findCalendarById = findCalendarByIdImpl
    findAllCalendarEntries = findAllCalendarEntriesImpl
    updateCalendar = updateCalendarImpl
    deleteCalendarEntry = deleteCalendarEntryImpl
    deleteTaskFromCalendarEntry = deleteTaskFromCalendarEntryImpl
    addTaskToCalendarEntry = addTaskToCalendarEntryImpl
