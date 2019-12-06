{-# LANGUAGE FlexibleContexts #-}

-- | Replacement for FoundationT from Happstack.Foundation
module HappstackHelper (FoundationT, AppState(..), getAcidSt) where

import           Control.Monad.State.Class      (MonadState, get)
import           Control.Monad.Trans.State.Lazy (StateT)

import           Happstack.Server               (ServerPartT)

data AppState acidState requestState  = AppState {
    acid    :: acidState
    , reqSt :: requestState
}

getAcidSt :: (Functor m, MonadState (AppState acidState requestState) m) => m acidState
getAcidSt = acid <$> get

type FoundationT acidState requestState   = StateT (AppState acidState requestState) (ServerPartT IO)
