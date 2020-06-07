{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
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
import qualified Server.HappstackHelper             as Foundation

instance CalendarDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query = Foundation.query
    findList = Foundation.query
    findListForRange = Foundation.query

findAllCalendarEntriesImpl :: (CalendarDAO m, MonadIO m) => User -> m [CalendarEntry]
findAllCalendarEntriesImpl = findList . CalendarEntryAcid.AllEntriesForUser

findAllCalendarEntriesWithinRangeImpl :: (CalendarDAO m, MonadIO m) => User -> UTCTime -> UTCTime -> m [CalendarEntry]
findAllCalendarEntriesWithinRangeImpl user start end = findListForRange (CalendarEntryAcid.AllEntriesForUserAndRange user start end)

createCalendarEntryImpl :: CalendarDAO m => CalendarEntry-> m CalendarEntry
createCalendarEntryImpl = create . CalendarEntryAcid.NewEntry

--TODO permission for delete
deleteCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> m ()
deleteCalendarEntryImpl = delete . CalendarEntryAcid.DeleteEntry . entryId

updateCalendarImpl :: (CalendarDAO m, AppContext m ) => CalendarEntry -> m (EitherResult CalendarEntry)
updateCalendarImpl calendarEntry = executeUnderUserPermission
    calendarEntry (update  $ CalendarEntryAcid.UpdateEntry calendarEntry)

findCalendarByIdImpl :: (CalendarDAO m, MonadIO m) => EntryId -> m (Maybe CalendarEntry)
findCalendarByIdImpl = query . CalendarEntryAcid.EntryById

deleteTaskFromCalendarEntryImpl :: (CalendarDAO m, AppContext m ) => CalendarEntry -> Int -> m (EitherResult CalendarEntry)
deleteTaskFromCalendarEntryImpl calendarEntry taskId =
    updateCalendarImpl calendarEntry {tasks = List.delete taskId (tasks calendarEntry)}

addTaskToCalendarEntryImpl :: (CalendarDAO m, AppContext m ) => CalendarEntry -> TaskId -> m (EitherResult CalendarEntry)
addTaskToCalendarEntryImpl calendarEntry taskId = updateCalendarImpl calendarEntry {tasks = taskId : tasks calendarEntry}

class Monad m => MonadDBCalendarRepo m where
    createCalendarEntry :: CalendarEntry -> m CalendarEntry
    findCalendarById :: EntryId -> m (Maybe CalendarEntry)
    findAllCalendarEntries :: User -> m [CalendarEntry]
    findAllCalendarEntriesWithinRange :: User -> UTCTime -> UTCTime -> m [CalendarEntry]
    updateCalendar :: CalendarEntry -> m (EitherResult CalendarEntry)
    deleteCalendarEntry :: CalendarEntry -> m ()
    deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m (EitherResult CalendarEntry)
    addTaskToCalendarEntry :: CalendarEntry -> TaskId -> m (EitherResult CalendarEntry)

instance CalendarDAO App => MonadDBCalendarRepo App where
    createCalendarEntry = createCalendarEntryImpl
    findCalendarById = findCalendarByIdImpl
    findAllCalendarEntries = findAllCalendarEntriesImpl
    findAllCalendarEntriesWithinRange = findAllCalendarEntriesWithinRangeImpl
    updateCalendar = updateCalendarImpl
    deleteCalendarEntry = deleteCalendarEntryImpl
    deleteTaskFromCalendarEntry = deleteTaskFromCalendarEntryImpl
    addTaskToCalendarEntry = addTaskToCalendarEntryImpl
