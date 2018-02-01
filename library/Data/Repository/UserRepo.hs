{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Repository.UserRepo
    ( MonadDBUser(..), deleteUser, updateName, addCalendarEntryToUser, addTaskToUser
    , deleteCalendarEntryFromUser, deleteTaskFromUser, getUser, createUser ) where

import Prelude
import Control.Monad.IO.Class
import Data.Maybe                        ( fromJust )

import qualified Data.List              as List
import qualified Happstack.Foundation   as Foundation

import Controller.AcidHelper            ( CtrlV' )
import Data.Domain.User                 ( User(..) )
import Data.Domain.Types                ( EntryId, TaskId, UserId )

import qualified Data.Repository.Acid.UserAcid       as UserAcid


class MonadDBUser m where
  create :: UserAcid.NewUser -> m User
  update :: UserAcid.UpdateUser -> m ()
  delete :: UserAcid.DeleteUser -> m ()
  query  :: UserAcid.UserById -> m (Maybe User)

instance MonadDBUser CtrlV' where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query


createUser :: MonadDBUser m => String -> m User
createUser name = let user = User { name = name
                    , userId = undefined
                    , calendarEntries = []
                    , belongingTasks = []
                    } in
        create $ UserAcid.NewUser user

deleteUser :: MonadDBUser m => User -> m ()
deleteUser user = delete $ UserAcid.DeleteUser (Data.Domain.User.userId user)

updateUser :: MonadDBUser m => User -> m ()
updateUser user = update $ UserAcid.UpdateUser user

updateName :: MonadDBUser m => User -> String -> m ()
updateName user newName = updateUser user {name = newName}

addCalendarEntryToUser :: MonadDBUser m => User -> EntryId -> m ()
addCalendarEntryToUser user entryId =
    updateUser user {calendarEntries = calendarEntries user ++ [entryId]}

deleteCalendarEntryFromUser :: MonadDBUser m =>
                            User -> EntryId -> m ()
deleteCalendarEntryFromUser user entryId =
    updateUser user {calendarEntries = List.delete entryId (calendarEntries user)}

addTaskToUser :: MonadDBUser m => User -> TaskId -> m ()
addTaskToUser user taskId =
    updateUser user {belongingTasks = belongingTasks user ++ [taskId]}

deleteTaskFromUser :: MonadDBUser m => User -> TaskId -> m ()
deleteTaskFromUser user taskId =
    updateUser user {belongingTasks = List.delete taskId (belongingTasks user)}

getUser :: (MonadDBUser m, MonadIO m) => UserId -> m User
getUser userId = do
    mUser <- query $ UserAcid.UserById userId
    return $ fromJust mUser