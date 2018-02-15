{-# LANGUAGE  TemplateHaskell, TypeFamilies, FlexibleInstances #-}

module Data.Repository.Acid.User
    ( MonadDBUser(..), initialUserListState, UserList(..), NewUser(..), UserById(..), AllUsers(..),
    GetUserList(..), UpdateUser(..), DeleteUser(..) ) where

import Data.Acid                       ( Query, Update, makeAcidic )
import Data.IxSet                      ( Indexable(..), ixFun, ixSet )

import Data.Domain.User         ( User(..) )
import Data.Domain.Types        ( UserId )

import qualified Data.Repository.Acid.InterfaceAcid      as   InterfaceAcid


instance Indexable User where
  empty = ixSet [ ixFun $ \bp -> [ userId bp ] ]

type UserList = InterfaceAcid.EntrySet User

initialUserListState :: UserList
initialUserListState = InterfaceAcid.initialState

getUserList :: Query UserList UserList
getUserList = InterfaceAcid.getEntrySet

-- create a new, empty user and add it to the database
newUser :: User -> Update UserList User
newUser = InterfaceAcid.newEntry

userById :: UserId -> Query UserList (Maybe User)
userById = InterfaceAcid.entryById

allUsers :: Query UserList [User]
allUsers = InterfaceAcid.allEntrysAsList

updateUser :: User -> Update UserList ()
updateUser = InterfaceAcid.updateEntry
            
deleteUser :: UserId -> Update UserList ()
deleteUser = InterfaceAcid.deleteEntry

$(makeAcidic ''UserList ['newUser, 'userById, 'allUsers, 'getUserList, 'updateUser, 'deleteUser])

class MonadDBUser m where
    create :: NewUser -> m User
    update :: UpdateUser -> m ()
    delete :: DeleteUser -> m ()
    query  :: UserById -> m (Maybe User)
