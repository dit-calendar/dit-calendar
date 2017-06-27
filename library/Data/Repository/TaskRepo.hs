{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.CalendarRepo where

import Happstack.Foundation       ( update, HasAcidState )
import Control.Monad.IO.Class
import Data.List                  ( delete )
import Data.Maybe                 ( fromJust )

import Data.Domain.User                       as Task
import Data.Repository.Acid.TaskAcid          as TaskAcid
import Data.Repository.Acid.CalendarAcid      as CalendarAcid
import Data.Domain.CalendarEntry              as CalendarEntry
import Data.Domain.Types          ( TaskId, EntryId )



createTask :: (HasAcidState m EntryList, HasAcidState m UserAcid.UserList,
            MonadIO m) => CalendarEntry -> String -> m ()
createEntry calendarEntry description =
    do
        mTask <- update (TaskAcid.NewTask description)
        addCalendarEntryToUser calendarEntry $ Task.taskId mTask

addTaskToCalendarEntry :: (HasAcidState m TaskAcid.CalendarEntry, MonadIO m) =>
    TaskId -> CalendarEntry -> m ()
addTaskToCalendarEntry taskId calendarEntry =
    let updatedCalendarEntry = calendarEntry {tasks = tasks calendarEntry ++ [taskId]} in
        update $ CalendarAcid.UpdateEntry updatedCalendarEntry
