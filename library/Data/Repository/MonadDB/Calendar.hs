{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances #-}

module Data.Repository.MonadDB.Calendar where

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.CalendarEntry  ( CalendarEntry )
import Data.Domain.User           ( User )
import Data.Domain.Types          ( EntryId, TaskId )

import Data.Repository.Acid.CalendarEntry        ( MonadDBCalendar )
import Data.Repository.Acid.Task                 ( MonadDBTask )
import Data.Repository.Acid.User                 ( MonadDBUser )

import qualified Data.Repository.CalendarRepo   as CalendarRepo

class Monad m => MonadDBCalendarRepo m where
    newCalendarEntry            :: String -> User -> m CalendarEntry
    deleteCalendarEntry         :: EntryId -> m ()
    deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m ()
    addTaskToCalendarEntry      :: CalendarEntry -> TaskId -> m ()

instance (MonadDBUser CtrlV', MonadDBCalendar CtrlV', MonadDBTask CtrlV')
        => MonadDBCalendarRepo CtrlV' where
    newCalendarEntry            = CalendarRepo.newCalendarEntry
    deleteCalendarEntry         = CalendarRepo.deleteCalendarEntry
    deleteTaskFromCalendarEntry = CalendarRepo.deleteTaskFromCalendarEntry
    addTaskToCalendarEntry      = CalendarRepo.addTaskToCalendarEntry