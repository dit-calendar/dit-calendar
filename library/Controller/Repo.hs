module Controller.Repo where

import Prelude hiding ( head )

import Domain.User as User

import Control.Exception.Lifted    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans  (MonadIO(..))
import Data.Acid
    ( AcidState(..), IsAcidic(..), openLocalState
    )
import Data.Acid.Local      ( createCheckpointAndClose
                            , openLocalStateFrom
                            )
import Data.Maybe           (fromMaybe)

data Acid = Acid
   { acidUserListState    :: AcidState UserList
   }

withLocalState
  :: ( MonadBaseControl IO m
     , MonadIO m
     , IsAcidic st
     , Typeable st
     ) =>
     Maybe FilePath        -- ^ path to state directory
  -> st                    -- ^ initial state value
  -> (AcidState st -> m a) -- ^ function which uses the
                           --   `AcidState` handle
  -> m a
withLocalState mPath initialState =
  bracket (liftIO $ open initialState)
          (liftIO . createCheckpointAndClose)
  where
    open = maybe openLocalState openLocalStateFrom mPath

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath action =
  let basePath = fromMaybe "_state" mBasePath
      countPath = Just $ basePath </> "userlist"
  in withLocalState countPath User.initialUserListState    $ \c ->
      action (Acid c g)
