{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.UserRepo
    ( deleteUserImpl, updateUserImpl, updateLoginNameImpl, addCalendarEntryToUserImpl, addTaskToUserImpl
    , deleteCalendarEntryFromUserImpl, deleteTaskFromUserImpl, findUserByIdImpl, createUserImpl, findUserByLoginNameIml,
     MonadDBUserRepo(..) ) where

import           Control.Monad.IO.Class
import           Data.Default              (def)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Prelude

import qualified Data.List                 as List
import qualified Happstack.Foundation      as Foundation

import           AppContext                (App)
import           Data.Domain.Task          (Task (..))
import           Data.Domain.Types         (EitherResult, EntryId, TaskId,
                                            UserId)
import           Data.Domain.User          (User (..))
import           Data.Repository.Acid.User (UserDAO (..))
import           Server.AcidInitializer

import qualified Data.Repository.Acid.User as UserAcid


instance UserDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query
    queryByLoginName = Foundation.query

createUserImpl :: UserDAO m => Text -> m User
createUserImpl name = let user = def { loginName = name
                    } in
        create $ UserAcid.NewUser user

deleteUserImpl :: UserDAO m => User -> m ()
deleteUserImpl =  delete . UserAcid.DeleteUser . userId

updateUserImpl :: UserDAO m => User -> m (EitherResult User)
updateUserImpl = update . UserAcid.UpdateUser

updateLoginNameImpl :: UserDAO m => User -> Text -> m (EitherResult User)
updateLoginNameImpl user newName = updateUserImpl user {loginName = newName}

addCalendarEntryToUserImpl :: UserDAO m => User -> EntryId -> m (EitherResult User)
addCalendarEntryToUserImpl user entryId =
    updateUserImpl user {ownerOfCalendarEntries = entryId : ownerOfCalendarEntries user}

deleteCalendarEntryFromUserImpl :: UserDAO m =>
                            User -> EntryId -> m (EitherResult User)
deleteCalendarEntryFromUserImpl user entryId =
    updateUserImpl user {ownerOfCalendarEntries = List.delete entryId (ownerOfCalendarEntries user)}

addTaskToUserImpl :: UserDAO m => User -> TaskId -> m (EitherResult User)
addTaskToUserImpl user taskId =
    updateUserImpl user {assignedToTasks = taskId : assignedToTasks user}

deleteTaskFromUserImpl :: UserDAO m => User -> Task -> m (EitherResult User)
deleteTaskFromUserImpl user task =
    updateUserImpl user {assignedToTasks = List.delete (taskId task) (assignedToTasks user)}

findUserByIdImpl :: (UserDAO m, MonadIO m) => UserId -> m (Maybe User)
findUserByIdImpl = query . UserAcid.UserById

findUserByLoginNameIml :: (UserDAO m, MonadIO m) => Text -> m (Maybe User)
findUserByLoginNameIml = queryByLoginName . UserAcid.FindByLoginName

class Monad m => MonadDBUserRepo m where
    createUser :: Text -> m User
    findUserById :: UserId -> m (Maybe User)
    updateUser :: User -> m (EitherResult User)
    deleteUser :: User -> m ()
    updateLoginName :: User -> Text -> m (EitherResult User)
    addCalendarEntryToUser :: User -> EntryId -> m (EitherResult User)
    deleteCalendarEntryFromUser :: User -> EntryId -> m (EitherResult User)
    addTaskToUser :: User -> TaskId -> m (EitherResult User)
    deleteTaskFromUser :: User -> Task -> m (EitherResult User)
    findUserByLoginName :: Text -> m (Maybe User)

instance UserDAO App => MonadDBUserRepo App where
    createUser = createUserImpl
    findUserById = findUserByIdImpl
    updateUser = updateUserImpl
    deleteUser = deleteUserImpl
    updateLoginName = updateLoginNameImpl
    addCalendarEntryToUser = addCalendarEntryToUserImpl
    deleteCalendarEntryFromUser = deleteCalendarEntryFromUserImpl
    addTaskToUser = addTaskToUserImpl
    deleteTaskFromUser = deleteTaskFromUserImpl
    findUserByLoginName = findUserByLoginNameIml
