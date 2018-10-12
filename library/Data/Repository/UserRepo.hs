{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.UserRepo
    ( deleteUserImpl, updateNameImpl, addCalendarEntryToUserImpl, addTaskToUserImpl
    , deleteCalendarEntryFromUserImpl, deleteTaskFromUserImpl, getUserImpl, createUserImpl, findUserByNameIml,
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
    queryByName = Foundation.query

createUserImpl :: UserDAO m => Text -> m User
createUserImpl name = let user = User { name = name
                    , userId = 0 --TODO why it can't be undefined if creating user with post interface?
                    , calendarEntries = []
                    , belongingTasks = []
                    } in
        create $ UserAcid.NewUser user

deleteUserImpl :: UserDAO m => User -> m ()
deleteUserImpl user = delete $ UserAcid.DeleteUser (Data.Domain.User.userId user)

updateUser :: UserDAO m => User -> m ()
updateUser user = update $ UserAcid.UpdateUser user

updateNameImpl :: UserDAO m => User -> Text -> m ()
updateNameImpl user newName = updateUser user {name = newName}

addCalendarEntryToUserImpl :: UserDAO m => User -> EntryId -> m ()
addCalendarEntryToUserImpl user entryId =
    updateUser user {calendarEntries = calendarEntries user ++ [entryId]}

deleteCalendarEntryFromUserImpl :: UserDAO m =>
                            User -> EntryId -> m ()
deleteCalendarEntryFromUserImpl user entryId =
    updateUser user {calendarEntries = List.delete entryId (calendarEntries user)}

addTaskToUserImpl :: UserDAO m => User -> TaskId -> m ()
addTaskToUserImpl user taskId =
    updateUser user {belongingTasks = belongingTasks user ++ [taskId]}

deleteTaskFromUserImpl :: UserDAO m => User -> TaskId -> m ()
deleteTaskFromUserImpl user taskId =
    updateUser user {belongingTasks = List.delete taskId (belongingTasks user)}

getUserImpl :: (UserDAO m, MonadIO m) => UserId -> m User
getUserImpl userId = do
    mUser <- query $ UserAcid.UserById userId
    return $ fromJust mUser

findUserByNameIml :: (UserDAO m, MonadIO m) => Text -> m (Maybe User)
findUserByNameIml name = queryByName $ UserAcid.FindByName name

class (UserDAO App) => MonadDBUserRepo m where
    createUser :: Text -> m User
    deleteUser :: User -> m ()
    updateName :: User -> Text -> m ()
    addCalendarEntryToUser :: User -> EntryId -> m ()
    deleteCalendarEntryFromUser :: User -> EntryId -> m ()
    addTaskToUser :: User -> TaskId -> m ()
    deleteTaskFromUser :: User -> TaskId -> m ()
    getUser :: UserId -> m User
    findUserByName :: Text -> m (Maybe User)

instance MonadDBUserRepo App where
    createUser = createUserImpl
    deleteUser = deleteUserImpl
    updateName = updateNameImpl
    addCalendarEntryToUser = addCalendarEntryToUserImpl
    deleteCalendarEntryFromUser = deleteCalendarEntryFromUserImpl
    addTaskToUser = addTaskToUserImpl
    deleteTaskFromUser = deleteTaskFromUserImpl
    getUser = getUserImpl
    findUserByName = findUserByNameIml
