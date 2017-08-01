{-# LANGUAGE FlexibleContexts #-}

module Data.Repository.CalendarTaskRepo where

import Happstack.Foundation     ( query, update, HasAcidState )
import Control.Monad.IO.Class
import Data.List                ( delete )
import Data.Maybe               ( fromJust )

import Data.Domain.CalendarEntry            as CalendarEntry
import Data.Domain.Types        ( UserId, EntryId )
import Data.Repository.TaskRepo             as TaskRepo
import Data.Repository.Acid.UserAcid        as UserAcid
import Data.Repository.Acid.TaskAcid        as TaskAcid
import Data.Repository.Acid.CalendarAcid    as CalendarAcid


deleteCalendarsTasks :: (HasAcidState m UserAcid.UserList, HasAcidState m TaskAcid.TaskList, MonadIO m)
    => CalendarEntry -> m ()
deleteCalendarsTasks calendar =
    foldr (\ x ->
      (>>) (do
        mTask <- query (TaskAcid.TaskById x)
        TaskRepo.deleteTask (fromJust mTask) ))
    (return ()) $ CalendarEntry.calendarTasks calendar
