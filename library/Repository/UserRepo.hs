{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies,
  RecordWildCards #-}

module Repository.UserRepo where

import Prelude                  hiding ( head )

import Control.Applicative      ( (<$>) )
import Control.Monad.Reader     ( ask )
import Control.Monad.State      ( get, put )
import Data.Data                ( Data, Typeable )
import Data.Acid                ( Query, Update, makeAcidic )
import Data.SafeCopy            ( base, deriveSafeCopy )
import Data.IxSet               ( Indexable(..), IxSet(..), (@=)
                                , Proxy(..), getOne, ixFun, ixSet
                                , toList, getEQ, insert )

import Domain.User              ( User(..) )
import Domain.Types             ( UserId )


instance Indexable User where
  empty = ixSet [ ixFun $ \bp -> [ userId bp ] ]

--type that represents the state we wish to store
data UserList = UserList
    { nextUserId :: UserId
    , users      :: IxSet User
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''UserList)

initialUserListState :: UserList
initialUserListState =
    UserList { nextUserId = 1
        , users      = empty
        }

getUserList :: Query UserList UserList
getUserList = ask

-- create a new, empty user and add it to the database
newUser :: String -> Update UserList User
newUser n =
    do  b@UserList{..} <- get
        let user = User { name = n
                        , userId  = nextUserId
                        , calendarEntrys = []
                        }
        --Because UserId is an instance of Enum we can use succ to increment it.
        put $ b { nextUserId = succ nextUserId
                , users      = insert user users
                }
        return user

userById :: UserId -> Query UserList (Maybe User)
userById uid = getOne . getEQ uid . users <$> ask

allUsers :: Query UserList [User]
allUsers = toList . users <$> ask

$(makeAcidic ''UserList ['newUser, 'userById, 'allUsers, 'getUserList])
