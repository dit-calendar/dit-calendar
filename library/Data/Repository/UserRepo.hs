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

import           Data.Domain.Types         (EntryId, TaskId, UserId)
import           Data.Domain.User          (User (..))
import           Data.Repository.Acid.User (UserDAO (..))
import           Presentation.AcidHelper   (App)

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
deleteUserImpl user = delete $ UserAcid.DeleteUser (Data.Domain.User.userId user)

updateUserImpl :: UserDAO m => User -> m (Either String User)
updateUserImpl user = update $ UserAcid.UpdateUser user

updateLoginNameImpl :: UserDAO m => User -> Text -> m (Either String User)
updateLoginNameImpl user newName = updateUserImpl user {loginName = newName}

addCalendarEntryToUserImpl :: UserDAO m => User -> EntryId -> m (Either String User)
addCalendarEntryToUserImpl user entryId =
    updateUserImpl user {calendarEntries = calendarEntries user ++ [entryId]}

deleteCalendarEntryFromUserImpl :: UserDAO m =>
                            User -> EntryId -> m (Either String User)
deleteCalendarEntryFromUserImpl user entryId =
    updateUserImpl user {calendarEntries = List.delete entryId (calendarEntries user)}

addTaskToUserImpl :: UserDAO m => User -> TaskId -> m (Either String User)
addTaskToUserImpl user taskId =
    updateUserImpl user {belongingTasks = belongingTasks user ++ [taskId]}

deleteTaskFromUserImpl :: UserDAO m => User -> TaskId -> m (Either String User)
deleteTaskFromUserImpl user taskId =
    updateUserImpl user {belongingTasks = List.delete taskId (belongingTasks user)}

findUserByIdImpl :: (UserDAO m, MonadIO m) => UserId -> m User
findUserByIdImpl userId = do
    mUser <- query $ UserAcid.UserById userId
    return $ fromJust mUser

findUserByLoginNameIml :: (UserDAO m, MonadIO m) => Text -> m (Maybe User)
findUserByLoginNameIml name = queryByLoginName $ UserAcid.FindByLoginName name

class (UserDAO App) => MonadDBUserRepo m where
    createUser :: Text -> m User
    findUserById :: UserId -> m User
    updateUser :: User -> m (Either String User)
    deleteUser :: User -> m ()
    updateLoginName :: User -> Text -> m (Either String User)
    addCalendarEntryToUser :: User -> EntryId -> m (Either String User)
    deleteCalendarEntryFromUser :: User -> EntryId -> m (Either String User)
    addTaskToUser :: User -> TaskId -> m (Either String User)
    deleteTaskFromUser :: User -> TaskId -> m (Either String User)

    findUserByLoginName :: Text -> m (Maybe User)

instance MonadDBUserRepo App where
    createUser = createUserImpl
    findUserById = findUserById
    updateUser = updateUserImpl
    deleteUser = deleteUserImpl
    updateLoginName = updateLoginNameImpl
    addCalendarEntryToUser = addCalendarEntryToUserImpl
    deleteCalendarEntryFromUser = deleteCalendarEntryFromUserImpl
    addTaskToUser = addTaskToUserImpl
    deleteTaskFromUser = deleteTaskFromUserImpl
    findUserByLoginName = findUserByLoginNameIml
