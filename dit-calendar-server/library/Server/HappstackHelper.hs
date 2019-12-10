{-# LANGUAGE FlexibleContexts #-}

-- | Replacement for FoundationT from Happstack.Foundation
module Server.HappstackHelper (FoundationT, AppState(..), runServerWithFoundationT, liftServerPartT2FoundationT) where

import           Control.Monad.Reader           (ReaderT, lift, runReaderT)

import           Happstack.Server               (ServerPartT)


newtype AppState requestState = AppState{
    reqSt :: requestState
    }

type FoundationT acidState = ReaderT acidState (ServerPartT IO)

runServerWithFoundationT :: FoundationT acidState a -> acidState -> ServerPartT IO a
runServerWithFoundationT = runReaderT

liftServerPartT2FoundationT :: ServerPartT IO response -> FoundationT acidState response
liftServerPartT2FoundationT = lift
