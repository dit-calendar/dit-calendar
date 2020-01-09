{-# LANGUAGE FlexibleContexts #-}

module Data.Service.Authorization (deleteAuthUser) where

import           Control.Monad.IO.Class
import           Data.Maybe                  (fromJust)
import           Happstack.Authenticate.Core (AuthenticateState)

import           Data.Domain.User            as DomainUser (User (..))
import           Server.HappstackHelper      (HasAcidState,
                                              query, update)

import qualified Happstack.Authenticate.Core as AuthUser

deleteAuthUser :: (Monad m, MonadIO m, HasAcidState m AuthenticateState) => DomainUser.User -> m ()
deleteAuthUser loggedUser =  do
    let userId = DomainUser.userId loggedUser
    mUser <- query (AuthUser.GetUserByUsername autUserName)
    update $ AuthUser.DeleteUser (AuthUser._userId $ fromJust mUser)
        where autUserName = AuthUser.Username {AuthUser._unUsername = loginName loggedUser}
