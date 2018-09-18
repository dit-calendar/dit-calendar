{-# LANGUAGE FlexibleContexts      #-}

module Data.Service.Authorization (deleteAuthUser) where

import           Control.Monad.IO.Class
import           Data.Maybe                           (fromJust)
import           Data.Text                            (pack, unpack)
import           Happstack.Authenticate.Core          (AuthenticateState)
import           Happstack.Foundation                 (HasAcidState (getAcidState),
                                                       query, update)

import           Data.Domain.User                     as DomainUser (User (..))

import qualified Happstack.Authenticate.Core          as AuthUser

deleteAuthUser :: (Monad m, MonadIO m, HasAcidState m AuthenticateState) => DomainUser.User -> m ()
deleteAuthUser loggedUser =  do
    let userId = DomainUser.userId loggedUser
    mUser <- query (AuthUser.GetUserByUsername autUserName)
    update $ AuthUser.DeleteUser (AuthUser._userId $ fromJust mUser)
        where autUserName = AuthUser.Username {AuthUser._unUsername = pack $ name loggedUser}