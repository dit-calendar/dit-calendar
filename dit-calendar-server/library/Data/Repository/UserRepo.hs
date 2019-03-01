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

import           AcidHelper                (App)
import           Data.Domain.Types         (EitherResponse, EntryId, TaskId,
                                            UserId)
import           Data.Domain.User          (User (..))
import           Data.Repository.Acid.User (UserDAO (..))

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

deleteUserImpl :: UserDAO m => UserId -> m ()
deleteUserImpl = delete . UserAcid.DeleteUser

updateUserImpl :: UserDAO m => User -> m (EitherResponse User)
updateUserImpl = update . UserAcid.UpdateUser

updateLoginNameImpl :: UserDAO m => User -> Text -> m (EitherResponse User)
updateLoginNameImpl user newName = updateUserImpl user {loginName = newName}

addCalendarEntryToUserImpl :: UserDAO m => User -> EntryId -> m (EitherResponse User)
addCalendarEntryToUserImpl user entryId =
    updateUserImpl user {calendarEntries = entryId : calendarEntries user}

deleteCalendarEntryFromUserImpl :: UserDAO m =>
                            User -> EntryId -> m (EitherResponse User)
deleteCalendarEntryFromUserImpl user entryId =
    updateUserImpl user {calendarEntries = List.delete entryId (calendarEntries user)}

addTaskToUserImpl :: UserDAO m => User -> TaskId -> m (EitherResponse User)
addTaskToUserImpl user taskId =
    updateUserImpl user {belongingTasks = taskId : belongingTasks user}

deleteTaskFromUserImpl :: UserDAO m => User -> TaskId -> m (EitherResponse User)
deleteTaskFromUserImpl user taskId =
    updateUserImpl user {belongingTasks = List.delete taskId (belongingTasks user)}

findUserByIdImpl :: (UserDAO m, MonadIO m) => UserId -> m (Maybe User)
findUserByIdImpl = query . UserAcid.UserById

findUserByLoginNameIml :: (UserDAO m, MonadIO m) => Text -> m (Maybe User)
findUserByLoginNameIml = queryByLoginName . UserAcid.FindByLoginName

class (UserDAO App) => MonadDBUserRepo m where
    createUser :: Text -> m User
    findUserById :: UserId -> m (Maybe User)
    updateUser :: User -> m (EitherResponse User)
    deleteUser :: UserId -> m ()
    updateLoginName :: User -> Text -> m (EitherResponse User)
    addCalendarEntryToUser :: User -> EntryId -> m (EitherResponse User)
    deleteCalendarEntryFromUser :: User -> EntryId -> m (EitherResponse User)
    addTaskToUser :: User -> TaskId -> m (EitherResponse User)
    deleteTaskFromUser :: User -> TaskId -> m (EitherResponse User)

    findUserByLoginName :: Text -> m (Maybe User)

instance MonadDBUserRepo App where
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
