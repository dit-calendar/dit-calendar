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
createUserImpl name = let user = User { loginName = name
                    , userId = 0 --TODO why it can't be undefined if creating user with post interface?
                    , calendarEntries = []
                    , belongingTasks = []
                    } in
        create $ UserAcid.NewUser user

deleteUserImpl :: UserDAO m => User -> m ()
deleteUserImpl user = delete $ UserAcid.DeleteUser (Data.Domain.User.userId user)

updateUserImpl :: UserDAO m => User -> m ()
updateUserImpl user = update $ UserAcid.UpdateUser user

updateLoginNameImpl :: UserDAO m => User -> Text -> m ()
updateLoginNameImpl user newName = updateUserImpl user {loginName = newName}

addCalendarEntryToUserImpl :: UserDAO m => User -> EntryId -> m ()
addCalendarEntryToUserImpl user entryId =
    updateUserImpl user {calendarEntries = calendarEntries user ++ [entryId]}

deleteCalendarEntryFromUserImpl :: UserDAO m =>
                            User -> EntryId -> m ()
deleteCalendarEntryFromUserImpl user entryId =
    updateUserImpl user {calendarEntries = List.delete entryId (calendarEntries user)}

addTaskToUserImpl :: UserDAO m => User -> TaskId -> m ()
addTaskToUserImpl user taskId =
    updateUserImpl user {belongingTasks = belongingTasks user ++ [taskId]}

deleteTaskFromUserImpl :: UserDAO m => User -> TaskId -> m ()
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
    deleteUser :: User -> m ()
    updateUser :: User -> m ()
    updateLoginName :: User -> Text -> m ()
    addCalendarEntryToUser :: User -> EntryId -> m ()
    deleteCalendarEntryFromUser :: User -> EntryId -> m ()
    addTaskToUser :: User -> TaskId -> m ()
    deleteTaskFromUser :: User -> TaskId -> m ()
    findUserById :: UserId -> m User
    findUserByLoginName :: Text -> m (Maybe User)

instance MonadDBUserRepo App where
    createUser = createUserImpl
    deleteUser = deleteUserImpl
    updateUser = updateUserImpl
    updateLoginName = updateLoginNameImpl
    addCalendarEntryToUser = addCalendarEntryToUserImpl
    deleteCalendarEntryFromUser = deleteCalendarEntryFromUserImpl
    addTaskToUser = addTaskToUserImpl
    deleteTaskFromUser = deleteTaskFromUserImpl
    findUserById = findUserById
    findUserByLoginName = findUserByLoginNameIml
