module Data.Repository.MonadDB.Task where

import Happstack.Foundation   as Foundation

import Controller.AcidHelper      ( App )
import Data.Domain.Task           ( Task )

import qualified Data.Repository.Acid.TaskAcid    as TaskAcid

class Monad m => MonadDBTask m where
  create :: TaskAcid.NewTask -> m Task
  update :: TaskAcid.UpdateTask -> m ()
  delete :: TaskAcid.DeleteTask -> m ()
  query  :: TaskAcid.TaskById -> m (Maybe Task)

instance MonadDBTask App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query
