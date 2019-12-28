{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Replacement for Foundation.hs from Happstack.Foundation
module Server.HappstackHelper (HasAcidState(..), FoundationT, AppState(..),
    runServerWithFoundationT, liftServerPartT2FoundationT, query, update) where

import           Control.Monad.Reader (MonadIO, ReaderT, lift, runReaderT)

import           Data.Acid            hiding (query, update)
import           Data.Acid.Advanced
import           Happstack.Server     (ServerPartT)

newtype AppState requestState = AppState{
    reqSt :: requestState
    }

type FoundationT acidState = ReaderT acidState (ServerPartT IO)

runServerWithFoundationT :: FoundationT acidState a -> acidState -> ServerPartT IO a
runServerWithFoundationT = runReaderT

liftServerPartT2FoundationT :: ServerPartT IO response -> FoundationT acidState response
liftServerPartT2FoundationT = lift


-- | 'HasAcidState' provides a single method 'getAcidState' which can be used to retrieve an 'AcidState' handle from the current monad.
class HasAcidState m st where
    getAcidState :: m (AcidState st)


-- | wrapper around query from acid-state
--
-- This variant automatically gets the 'AcidState' handle from the monad
query :: forall event m.
         ( Functor m
         , MonadIO m
         , QueryEvent event
         , HasAcidState m (EventState event)
         ) =>
         event
      -> m (EventResult event)
query event =
    do as <- getAcidState
       query' (as :: AcidState (EventState event)) event

-- | wrapper around update from acid-state
--
-- This variant automatically gets the 'AcidState' handle from the monad
update :: forall event m.
          ( Functor m
          , MonadIO m
          , UpdateEvent event
          , HasAcidState m (EventState event)
          ) =>
          event
       -> m (EventResult event)
update event =
    do as <- getAcidState
       update' (as :: AcidState (EventState event)) event
