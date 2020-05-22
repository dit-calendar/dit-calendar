{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Repository.Acid.User
    ( UserDAO(..), initialUserListState, UserList(..), NewUser(..), UserById(..), AllUsers(..),
    GetUserList(..), UpdateUser(..), DeleteUser(..), FindByLoginName(..), FindByTelegramToken(..) ) where

import           Control.Monad.Reader               (ask)
import           Data.Acid                          (Query, Update, makeAcidic)
import           Data.IxSet                         (Indexable (..), getOne,
                                                     ixFun, ixSet, (@=))
import           Data.Text                          (Text)

import           Data.Domain.Types                  (EitherResult,
                                                     TelegramToken,
                                                     TelegramTokenIndex (..),
                                                     UserId)
import           Data.Domain.User                   (User (..))

import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid


instance Indexable User where
  empty = ixSet [ ixFun $ \bp -> [ userId bp ],
                  ixFun $ \bp -> [ loginName bp ],
                  -- TODO: Unique
                  ixFun $ \bp -> [ TelegramTokenIndex $ telegramToken bp]]

type UserList = InterfaceAcid.EntrySet User UserId

initialUserListState :: UserList
initialUserListState = InterfaceAcid.initialState 1

getUserList :: Query UserList UserList
getUserList = InterfaceAcid.getEntrySet

-- create a new, empty user and add it to the database
newUser :: User -> Update UserList User
newUser = InterfaceAcid.newEntry

userById :: UserId -> Query UserList (Maybe User)
userById = InterfaceAcid.entryById

findByLoginName :: Text -> Query UserList (Maybe User)
findByLoginName loginName = do InterfaceAcid.EntrySet{..} <- ask
                               return $ getOne $ entrys @= loginName

findByTelegramToken :: TelegramToken -> Query UserList (Maybe User)
findByTelegramToken token = do InterfaceAcid.EntrySet{..} <- ask
                               return $ getOne $ entrys @= TelegramTokenIndex token

allUsers :: Query UserList [User]
allUsers = InterfaceAcid.allEntrysAsList

updateUser :: User -> Update UserList (EitherResult User)
updateUser = InterfaceAcid.updateEntry

deleteUser :: UserId -> Update UserList ()
deleteUser = InterfaceAcid.deleteEntry

$(makeAcidic ''UserList ['newUser, 'userById, 'findByLoginName, 'allUsers, 'getUserList, 'updateUser, 'deleteUser, 'findByTelegramToken])

class Monad m => UserDAO m where
    create :: NewUser -> m User
    update :: UpdateUser -> m (EitherResult User)
    delete :: DeleteUser -> m ()
    query  :: UserById -> m (Maybe User)
    queryByLoginName :: FindByLoginName -> m (Maybe User)
