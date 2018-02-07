{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances #-}

module Data.Service.MonadDB.Calendar where

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.CalendarEntry  ( CalendarEntry )
import Data.Domain.User           ( User )
import Data.Domain.Types          ( EntryId, TaskId )

import Data.Repository.Acid.MonadDB.CalendarEntry    ( MonadDBCalendar )
import Data.Repository.Acid.MonadDB.Task             ( MonadDBTask )
import Data.Repository.Acid.MonadDB.User             ( MonadDBUser )

import qualified Data.Repository.CalendarRepo   as CalendarRepo

class MonadDBCalendarService m where
    newCalendarEntry            :: String -> User -> m CalendarEntry
    deleteCalendarEntry         :: EntryId -> m ()
    deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m ()
    addTaskToCalendarEntry      :: CalendarEntry -> TaskId -> m ()

instance (MonadDBUser CtrlV', MonadDBCalendar CtrlV', MonadDBTask CtrlV')
        => MonadDBCalendarService CtrlV' where
    newCalendarEntry            = CalendarRepo.newCalendarEntry
    deleteCalendarEntry         = CalendarRepo.deleteCalendarEntry
    deleteTaskFromCalendarEntry = CalendarRepo.deleteTaskFromCalendarEntry
    addTaskToCalendarEntry      = CalendarRepo.addTaskToCalendarEntry