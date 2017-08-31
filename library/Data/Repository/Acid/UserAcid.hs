{-# LANGUAGE  TemplateHaskell, TypeFamilies,
  RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

module Data.Repository.Acid.UserAcid 
    (initialUserListState, UserList(..), NewUser(..), UserById(..), AllUsers(..),
    GetUserList(..), UpdateUser(..), DeleteUser(..)) where

import Control.Monad.State             ( get, put )
import Data.Acid                       ( Query, Update, makeAcidic )
import Data.IxSet                      ( Indexable(..), ixFun, ixSet, insert )

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
newUser :: String -> Update UserList User
newUser n =
    do  b@InterfaceAcid.EntrySet{..} <- get
        let nextUserId = nextEntryId
            user = User { name = n
                        , userId  = nextUserId
                        , calendarEntries = []
                        , belongingTasks = []
                        }
        --Because UserId is an instance of Enum we can use succ to increment it.
        put $ b { InterfaceAcid.nextEntryId = succ nextUserId
                , InterfaceAcid.entrys      = insert user entrys
                }
        return user

userById :: UserId -> Query UserList (Maybe User)
userById = InterfaceAcid.entryById

allUsers :: Query UserList [User]
allUsers = InterfaceAcid.allEntrysAsList

updateUser :: User -> Update UserList ()
updateUser updatedUser = InterfaceAcid.updateEntry updatedUser userId
            
deleteUser :: UserId -> Update UserList ()
deleteUser = InterfaceAcid.deleteEntry

$(makeAcidic ''UserList ['newUser, 'userById, 'allUsers, 'getUserList, 'updateUser, 'deleteUser])
