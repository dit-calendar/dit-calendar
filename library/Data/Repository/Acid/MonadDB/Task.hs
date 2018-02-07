{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Repository.Acid.MonadDB.Task ( MonadDBTask(..) ) where

import Happstack.Foundation   as Foundation

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.Task           ( Task )

import qualified Data.Repository.Acid.Task    as TaskAcid

class Monad m => MonadDBTask m where
    create :: TaskAcid.NewTask -> m Task
    update :: TaskAcid.UpdateTask -> m ()
    delete :: TaskAcid.DeleteTask -> m ()
    query  :: TaskAcid.TaskById -> m (Maybe Task)
        
instance MonadDBTask CtrlV' where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query