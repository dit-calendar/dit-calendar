{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances, MonoLocalBinds #-}

module Data.Repository.UserRepo
    ( deleteUserImpl, updateNameImpl, addCalendarEntryToUserImpl, addTaskToUserImpl
    , deleteCalendarEntryFromUserImpl, deleteTaskFromUserImpl, getUserImpl, createUserImpl,
     MonadDBUserRepo(..) ) where

import Prelude
import Control.Monad.IO.Class
import Data.Maybe                        ( fromJust )

import qualified Data.List              as List
import qualified Happstack.Foundation   as Foundation

import Controller.AcidHelper            ( CtrlV' )
import Data.Domain.User                 ( User(..) )
import Data.Domain.Types                ( EntryId, TaskId, UserId )

import Data.Repository.Acid.User                 ( MonadDBUser(..) )
import Data.Repository.Acid.CalendarEntry        ( MonadDBCalendar )
import Data.Repository.Acid.Task                 ( MonadDBTask )

import qualified Data.Repository.Acid.User       as UserAcid


instance MonadDBUser CtrlV' where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

createUserImpl :: MonadDBUser m => String -> m User
createUserImpl name = let user = User { name = name
                    , userId = 0 --TODO why it can't be undefined if creating user with post interface?
                    , calendarEntries = []
                    , belongingTasks = []
                    } in
        create $ UserAcid.NewUser user

deleteUserImpl :: MonadDBUser m => User -> m ()
deleteUserImpl user = delete $ UserAcid.DeleteUser (Data.Domain.User.userId user)

updateUser :: MonadDBUser m => User -> m ()
updateUser user = update $ UserAcid.UpdateUser user

updateNameImpl :: MonadDBUser m => User -> String -> m ()
updateNameImpl user newName = updateUser user {name = newName}

addCalendarEntryToUserImpl :: MonadDBUser m => User -> EntryId -> m ()
addCalendarEntryToUserImpl user entryId =
    updateUser user {calendarEntries = calendarEntries user ++ [entryId]}

deleteCalendarEntryFromUserImpl :: MonadDBUser m =>
                            User -> EntryId -> m ()
deleteCalendarEntryFromUserImpl user entryId =
    updateUser user {calendarEntries = List.delete entryId (calendarEntries user)}

addTaskToUserImpl :: MonadDBUser m => User -> TaskId -> m ()
addTaskToUserImpl user taskId =
    updateUser user {belongingTasks = belongingTasks user ++ [taskId]}

deleteTaskFromUserImpl :: MonadDBUser m => User -> TaskId -> m ()
deleteTaskFromUserImpl user taskId =
    updateUser user {belongingTasks = List.delete taskId (belongingTasks user)}

getUserImpl :: (MonadDBUser m, MonadIO m) => UserId -> m User
getUserImpl userId = do
    mUser <- query $ UserAcid.UserById userId
    return $ fromJust mUser

class MonadDBUserRepo m where
    createUser :: String -> m User
    deleteUser :: User -> m ()
    updateName :: User -> String -> m ()
    addCalendarEntryToUser :: User -> EntryId -> m ()
    deleteCalendarEntryFromUser :: User -> EntryId -> m ()
    addTaskToUser :: User -> TaskId -> m ()
    deleteTaskFromUser :: User -> TaskId -> m ()
    getUser :: UserId -> m User

instance (MonadDBUser CtrlV', MonadDBCalendar CtrlV', MonadDBTask CtrlV')
        => MonadDBUserRepo CtrlV' where
    createUser = createUserImpl
    deleteUser = deleteUserImpl
    updateName = updateNameImpl
    addCalendarEntryToUser = addCalendarEntryToUserImpl
    deleteCalendarEntryFromUser = deleteCalendarEntryFromUserImpl
    addTaskToUser = addTaskToUserImpl
    deleteTaskFromUser = deleteTaskFromUserImpl
    getUser = getUserImpl