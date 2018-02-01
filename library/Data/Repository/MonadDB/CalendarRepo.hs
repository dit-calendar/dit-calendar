{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Repository.MonadDB.CalendarRepo where

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.CalendarEntry  ( CalendarEntry )
import Data.Domain.User           ( User )
import Data.Domain.Types          ( EntryId, TaskId )

import qualified Data.Repository.CalendarRepo   as CalendarRepo

class Monad m => MonadDBCalendarRepo m where
  createEntry                 :: String -> User -> m CalendarEntry
  deleteCalendar              :: EntryId -> m ()
  updateDescription           :: CalendarEntry -> String -> m ()
  deleteTaskFromCalendarEntry :: CalendarEntry -> Int -> m ()
  addTaskToCalendarEntry      :: CalendarEntry -> TaskId -> m ()

instance MonadDBCalendarRepo CtrlV' where
    createEntry                 = CalendarRepo.createEntry
    deleteCalendar              = CalendarRepo.deleteCalendar
    updateDescription           = CalendarRepo.updateDescription
    deleteTaskFromCalendarEntry = CalendarRepo.deleteTaskFromCalendarEntry
    addTaskToCalendarEntry      = CalendarRepo.addTaskToCalendarEntry