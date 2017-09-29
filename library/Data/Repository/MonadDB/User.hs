{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, FlexibleContexts #-}

module Data.Repository.MonadDB.User where

import HSP.XMLGenerator                       ( XMLGenT )
import Happstack.Foundation   as Foundation   ( update, query, FoundationT', HasAcidState )

import Controller.AcidHelper      ( Acid )
import Route.PageEnum             ( SiteMap )

import Data.Domain.User           ( User )
import qualified Data.Repository.Acid.UserAcid    as UserAcid

class (Monad m, HasAcidState m UserAcid.UserList) => MonadDBUser m where
  create :: UserAcid.NewUser -> m User
  update :: UserAcid.UpdateUser -> m ()
  delete :: UserAcid.DeleteUser -> m ()
  query  :: UserAcid.UserById -> m (Maybe User)

instance MonadDBUser (XMLGenT (FoundationT' SiteMap Acid () IO)) where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query
