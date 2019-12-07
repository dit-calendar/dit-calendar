{-# LANGUAGE FlexibleContexts #-}

-- | Replacement for FoundationT from Happstack.Foundation
module HappstackHelper (FoundationT, AppState(..), runServerWithFoundationT, liftServerPartT2FoundationT) where

import           Control.Monad.Reader           (ReaderT, lift, runReaderT)
import           Control.Monad.Trans.State.Lazy (StateT, evalStateT)

import           Happstack.Server               (ServerPartT)


newtype AppState requestState = AppState{
    reqSt :: requestState
    }

type FoundationT acidState requestState = StateT (AppState requestState) (ReaderT acidState (ServerPartT IO))


runServerWithState :: FoundationT acidState requestState a -> requestState -> ReaderT acidState (ServerPartT IO) a
runServerWithState app = evalStateT app . AppState

runServerWithFoundationT :: FoundationT acidState requestState a -> requestState -> acidState -> ServerPartT IO a
runServerWithFoundationT app = runReaderT . runServerWithState app

liftServerPartT2FoundationT :: ServerPartT IO response -> FoundationT acidState requestState response
liftServerPartT2FoundationT = lift . lift
