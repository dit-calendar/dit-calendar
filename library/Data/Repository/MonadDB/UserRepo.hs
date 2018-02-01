{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Repository.MonadDB.UserRepo where

import Data.Domain.Types          ( TaskId, EntryId, UserId )
import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.User           ( User )

import qualified Data.Repository.UserRepo as UseRepo

class Monad m => MonadDBUserHelper m where
    createUser :: String -> m User
    deleteUser :: User -> m ()
    updateName :: User -> String -> m ()
    addCalendarEntryToUser :: User -> EntryId -> m ()
    deleteCalendarEntryFromUser :: User -> EntryId -> m ()
    addTaskToUser :: User -> TaskId -> m ()
    deleteTaskFromUser :: User -> TaskId -> m ()
    getUser :: UserId -> m User

instance MonadDBUserHelper CtrlV' where
    createUser = UseRepo.createUser
    deleteUser = UseRepo.deleteUser
    updateName = UseRepo.updateName
    addCalendarEntryToUser = UseRepo.addCalendarEntryToUser
    deleteCalendarEntryFromUser = UseRepo.deleteCalendarEntryFromUser
    addTaskToUser = UseRepo.addTaskToUser
    deleteTaskFromUser = UseRepo.deleteTaskFromUser
    getUser = UseRepo.getUser