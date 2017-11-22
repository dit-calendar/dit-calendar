module Data.Repository.MonadDB.User where

import Happstack.Foundation   as Foundation

import Controller.AcidHelper      ( App )
import Data.Domain.User           ( User )

import qualified Data.Repository.Acid.UserAcid    as UserAcid

class Monad m => MonadDBUser m where
  create :: UserAcid.NewUser -> m User
  update :: UserAcid.UpdateUser -> m ()
  delete :: UserAcid.DeleteUser -> m ()
  query  :: UserAcid.UserById -> m (Maybe User)

instance MonadDBUser App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query
