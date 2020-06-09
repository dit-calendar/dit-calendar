{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.UserRepo
    ( deleteUserImpl, updateUserImpl, addCalendarEntryToUserImpl
    , deleteCalendarEntryFromUserImpl, findUserByIdImpl, createUserImpl, findUserByLoginNameIml,
     MonadDBUserRepo(..) ) where

import           Control.Monad.IO.Class
import           Data.Text                 (Text)
import           Prelude

import qualified Data.List                 as List

import           AppContext                (App)
import           Data.Domain.Types         (EitherResult, EntryId, UserId)
import           Data.Domain.User          (User (..))
import           Data.Repository.Acid.User (UserDAO (..))
import           Server.AcidInitializer

import qualified Data.Repository.Acid.User as UserAcid
import qualified Server.HappstackHelper    as Foundation


instance UserDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query
    queryByLoginName = Foundation.query

createUserImpl :: UserDAO m => User -> m User
createUserImpl user = create $ UserAcid.NewUser user

deleteUserImpl :: UserDAO m => User -> m ()
deleteUserImpl =  delete . UserAcid.DeleteUser . userId

updateUserImpl :: UserDAO m => User -> m (EitherResult User)
updateUserImpl = update . UserAcid.UpdateUser

addCalendarEntryToUserImpl :: UserDAO m => User -> EntryId -> m (EitherResult User)
addCalendarEntryToUserImpl user entryId =
    updateUserImpl user {ownerOfCalendarEntries = entryId : ownerOfCalendarEntries user}

deleteCalendarEntryFromUserImpl :: UserDAO m =>
                            User -> EntryId -> m (EitherResult User)
deleteCalendarEntryFromUserImpl user entryId =
    updateUserImpl user {ownerOfCalendarEntries = List.delete entryId (ownerOfCalendarEntries user)}

findUserByIdImpl :: (UserDAO m, MonadIO m) => UserId -> m (Maybe User)
findUserByIdImpl = query . UserAcid.UserById

findUserByLoginNameIml :: (UserDAO m, MonadIO m) => Text -> m (Maybe User)
findUserByLoginNameIml = queryByLoginName . UserAcid.FindByLoginName

class Monad m => MonadDBUserRepo m where
    createUser :: User -> m User
    findUserById :: UserId -> m (Maybe User)
    updateUser :: User -> m (EitherResult User)
    deleteUser :: User -> m ()
    addCalendarEntryToUser :: User -> EntryId -> m (EitherResult User)
    deleteCalendarEntryFromUser :: User -> EntryId -> m (EitherResult User)
    findUserByLoginName :: Text -> m (Maybe User)

instance UserDAO App => MonadDBUserRepo App where
    createUser = createUserImpl
    findUserById = findUserByIdImpl
    updateUser = updateUserImpl
    deleteUser = deleteUserImpl
    addCalendarEntryToUser = addCalendarEntryToUserImpl
    deleteCalendarEntryFromUser = deleteCalendarEntryFromUserImpl
    findUserByLoginName = findUserByLoginNameIml
