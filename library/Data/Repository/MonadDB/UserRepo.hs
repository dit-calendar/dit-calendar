{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances #-}

module Data.Repository.MonadDB.UserRepo where

import Data.Domain.Types             ( TaskId, EntryId, UserId )
import Controller.AcidHelper         ( CtrlV' )
import Data.Domain.User              ( User )

import Data.Repository.MonadDB.User      ( MonadDBUser )
import Data.Repository.MonadDB.Calendar  ( MonadDBCalendar )
import Data.Repository.MonadDB.Task      ( MonadDBTask )

import qualified Data.Repository.UserRepo as UseRepo

class MonadDBUserHelper m where
    createUser :: String -> m User
    deleteUser :: User -> m ()
    updateName :: User -> String -> m ()
    addCalendarEntryToUser :: User -> EntryId -> m ()
    deleteCalendarEntryFromUser :: User -> EntryId -> m ()
    addTaskToUser :: User -> TaskId -> m ()
    deleteTaskFromUser :: User -> TaskId -> m ()
    getUser :: UserId -> m User

instance (MonadDBUser CtrlV', MonadDBCalendar CtrlV', MonadDBTask CtrlV', Monad CtrlV') => MonadDBUserHelper CtrlV' where
    createUser = UseRepo.createUser
    deleteUser = UseRepo.deleteUser
    updateName = UseRepo.updateName
    addCalendarEntryToUser = UseRepo.addCalendarEntryToUser
    deleteCalendarEntryFromUser = UseRepo.deleteCalendarEntryFromUser
    addTaskToUser = UseRepo.addTaskToUser
    deleteTaskFromUser = UseRepo.deleteTaskFromUser
    getUser = UseRepo.getUser