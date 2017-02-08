{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Repository.UserRepository where

import Prelude hiding ( head )

import Control.Applicative  ( (<$>) )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Data.Acid            ( Query, Update, makeAcidic )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.IxSet           ( Indexable(..), IxSet(..), (@=)
                            , Proxy(..), getOne, ixFun, ixSet
                            , toList, getEQ )
import qualified Data.IxSet as IxSet

import Domain.User as User


instance Indexable User.UserState where
  empty = ixSet [ ixFun $ \bp -> [ userId bp ] ]

data UserList = UserList
    { nextUserId :: Integer
    , users      :: IxSet User.UserState
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''UserList)

initialUserListState :: UserList
initialUserListState =
    UserList { nextUserId = 1
        , users      = empty
        }

-- create a new, empty user and add it to the database
newUser :: String -> Update UserList User.UserState
newUser n =
    do  b@UserList{..} <- get
        let user = UserState { name = n
                        , userId  = nextUserId
                        }
        --Because UserId is an instance of Enum we can use succ to increment it.
        put $ b { nextUserId = succ nextUserId
                , users      = IxSet.insert user users
                }
        return user

userById :: Integer -> Query UserList (Maybe User.UserState)
userById uid = getOne . getEQ uid . users <$> ask

allUsers :: Query UserList [User.UserState]
allUsers = toList . users <$> ask

$(makeAcidic ''UserList ['newUser, 'userById, 'allUsers])
