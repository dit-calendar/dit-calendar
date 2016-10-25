{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Domain.User where

import Prelude hiding ( head )

import Control.Applicative  ( (<$>) )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Data.Acid            ( Query, Update, makeAcidic )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.IxSet           ( Indexable(..), IxSet(..), (@=)
                            , Proxy(..), getOne, ixFun, ixSet )
import qualified Data.IxSet as IxSet

import Web.Routes ( PathInfo(..))

--type that represents the state we wish to store
data UserState = UserState { name :: String, userId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserState)

instance Indexable UserState where
  empty = ixSet [ ixFun $ \bp -> [ userId bp ] ]

initialUserState :: UserState
initialUserState = UserState "" 1

--update function of UserState
setName :: String -> Update UserState String
setName n =
    do c@UserState{..} <- get
       let newName = name ++ n
       put $ c { name = newName }
       return newName

--read function of UserState
peekName :: Query UserState String
peekName = name <$> ask

--turn the update and read functions into acid-state events
$(makeAcidic ''UserState ['setName, 'peekName])


data UserList = UserList
    { nextUserId :: Integer
    , users      :: IxSet UserState
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''UserList)

initialUserListState :: UserList
initialUserListState =
    UserList { nextUserId = 1
         , users      = empty
         }

userById :: Integer -> Query UserList (Maybe UserState)
userById uid =
     do UserList{..} <- ask
        return $ getOne $ users @= uid

$(makeAcidic ''UserList ['userById])
