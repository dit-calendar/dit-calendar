{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances #-}

module Data.Repository.MonadDB.CalendarRepo where

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.CalendarEntry  ( CalendarEntry )
import Data.Domain.User           ( User )
import Data.Domain.Types          ( EntryId, TaskId )

import Data.Repository.MonadDB.User      ( MonadDBUser )
import Data.Repository.MonadDB.Calendar  ( MonadDBCalendar )
import Data.Repository.MonadDB.Task      ( MonadDBTask )

import qualified Data.Repository.CalendarRepo   as CalendarRepo

class MonadDBCalendarRepo m where
  createEntry                 :: String -> User -> m CalendarEntry
  deleteCalendar              :: EntryId -> m ()
  updateDescription           :: CalendarEntry -> String -> m ()
  deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m ()
  addTaskToCalendarEntry      :: CalendarEntry -> TaskId -> m ()

instance (MonadDBUser CtrlV', MonadDBCalendar CtrlV', MonadDBTask CtrlV')
        => MonadDBCalendarRepo CtrlV' where
    createEntry                 = CalendarRepo.createEntry
    deleteCalendar              = CalendarRepo.deleteCalendar
    updateDescription           = CalendarRepo.updateDescription
    deleteTaskFromCalendarEntry = CalendarRepo.deleteTaskFromCalendarEntry
    addTaskToCalendarEntry      = CalendarRepo.addTaskToCalendarEntry