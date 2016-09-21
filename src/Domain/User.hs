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

import Web.Routes ( PathInfo(..))
import Web.Routes.TH  (derivePathInfo)

newtype UserId = UserId { unUserId :: Int }
    deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

--A url type
data Sitemap
  = Home
  | User UserId
$(derivePathInfo ''Sitemap)

--type that represents the state we wish to store
data UserState = UserState { name :: String }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserState)

initialUserState :: UserState
initialUserState = UserState ""

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
