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
import           Presentation.AcidHelper            (App)

import qualified Data.Repository.Acid.CalendarEntry as CalendarEntryAcid

instance CalendarDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query = Foundation.query

createCalendarEntryImpl :: CalendarDAO m => UTCTime -> Description -> User -> m CalendarEntry
createCalendarEntryImpl newDate description user =
    let entry =
            def
                { description = description
                , CalendarEntry.userId = User.userId user
                , date = newDate
                }
     in create (CalendarEntryAcid.NewEntry entry)

deleteCalendarEntryImpl :: CalendarDAO m => EntryId -> m ()
deleteCalendarEntryImpl entryId = delete $ CalendarEntryAcid.DeleteEntry entryId

updateCalendarImpl :: CalendarDAO m => CalendarEntry -> m (Either String CalendarEntry)
updateCalendarImpl calendarEntry = update $ CalendarEntryAcid.UpdateEntry calendarEntry

findCalendarByIdImpl :: (CalendarDAO m, MonadIO m) => EntryId -> m CalendarEntry
findCalendarByIdImpl entryId =
    fromJust <$> query (CalendarEntryAcid.EntryById entryId)

deleteTaskFromCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> Int -> m (Either String CalendarEntry)
deleteTaskFromCalendarEntryImpl calendarEntry taskId =
    updateCalendarImpl calendarEntry {tasks = List.delete taskId (tasks calendarEntry)}

addTaskToCalendarEntryImpl :: CalendarDAO m => CalendarEntry -> TaskId -> m (Either String CalendarEntry)
addTaskToCalendarEntryImpl calendarEntry taskId = updateCalendarImpl calendarEntry {tasks = tasks calendarEntry ++ [taskId]}

class (Monad m, CalendarDAO App) =>
      MonadDBCalendarRepo m
    where
    createCalendarEntry :: UTCTime -> Description -> User -> m CalendarEntry
    findCalendarById :: EntryId -> m CalendarEntry
    updateCalendar :: CalendarEntry -> m (Either String CalendarEntry)
    deleteCalendarEntry :: EntryId -> m ()
    deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m (Either String CalendarEntry)
    addTaskToCalendarEntry :: CalendarEntry -> TaskId -> m (Either String CalendarEntry)

instance MonadDBCalendarRepo App where
    createCalendarEntry = createCalendarEntryImpl
    findCalendarById = findCalendarByIdImpl
    updateCalendar = updateCalendarImpl
    deleteCalendarEntry = deleteCalendarEntryImpl
    deleteTaskFromCalendarEntry = deleteTaskFromCalendarEntryImpl
    addTaskToCalendarEntry = addTaskToCalendarEntryImpl
