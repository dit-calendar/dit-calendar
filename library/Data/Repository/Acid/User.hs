{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Repository.Acid.User
    ( MonadDBUser(..), initialUserListState, UserList(..), NewUser(..), UserById(..), AllUsers(..),
    GetUserList(..), UpdateUser(..), DeleteUser(..), FindByName(..) ) where

import           Data.Acid                          (Query, Update, makeAcidic)
import           Data.IxSet                         (Indexable (..), ixFun,  getEQ, getOne,
                                                     ixSet, (@=), toList)
import           Control.Applicative                ((<$>))
import           Control.Monad.Reader               (ask)
import           Data.Domain.Types                  (UserId)
import           Data.Domain.User                   (User (..))

import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid


instance Indexable User where
  empty = ixSet [ ixFun $ \bp -> [ userId bp ],
                  ixFun $ \bp -> [ name bp ] ]

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

findByName :: String -> Query UserList (Maybe User)
findByName name = do b@InterfaceAcid.EntrySet{..} <- ask
                     return $ getOne $ entrys @= name

allUsers :: Query UserList [User]
allUsers = InterfaceAcid.allEntrysAsList

updateUser :: User -> Update UserList ()
updateUser = InterfaceAcid.updateEntry

deleteUser :: UserId -> Update UserList ()
deleteUser = InterfaceAcid.deleteEntry

$(makeAcidic ''UserList ['newUser, 'userById, 'findByName, 'allUsers, 'getUserList, 'updateUser, 'deleteUser])

class MonadDBUser m where
    create :: NewUser -> m User
    update :: UpdateUser -> m ()
    delete :: DeleteUser -> m ()
    query  :: UserById -> m (Maybe User)
    queryByName :: FindByName -> m (Maybe User)
