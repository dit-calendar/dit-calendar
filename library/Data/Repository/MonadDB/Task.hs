{-# LANGUAGE FlexibleInstances #-}

module Data.Repository.MonadDB.Task where

import Control.Monad.IO.Class
import HSP.XMLGenerator                       ( XMLGenT )
import Happstack.Foundation   as Foundation   ( update, query, FoundationT' )

import Controller.AcidHelper      ( Acid )
import Route.PageEnum             ( SiteMap )

import Data.Domain.Task        ( Task )
import qualified Data.Repository.Acid.TaskAcid    as TaskAcid

class Monad m => MonadDBTask m where
  create :: TaskAcid.NewTask -> m Task
  update :: TaskAcid.UpdateTask -> m ()
  delete :: TaskAcid.DeleteTask -> m ()
  query  :: TaskAcid.TaskById -> m (Maybe Task)

instance MonadDBTask (XMLGenT (FoundationT' SiteMap Acid () IO)) where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query
