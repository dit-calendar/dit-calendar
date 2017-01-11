{-# LANGUAGE FlexibleContexts
  , MultiParamTypeClasses
  , OverloadedStrings, ScopedTypeVariables
  , TypeFamilies, FlexibleInstances #-}

module Controller.Repo where

import Domain.User as User

import Prelude hiding       (head, id)
import System.FilePath      ((</>))

import Control.Exception.Base    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad        (MonadPlus, mplus)
import Control.Monad.Reader ( MonadReader, ReaderT(..)
                            , ask)
import Control.Monad.Trans  (MonadIO(..))
import Data.Acid
    ( AcidState(..), IsAcidic(..), openLocalState
    )
import Data.Acid.Local      ( createCheckpointAndClose
                            , openLocalStateFrom
                            )
import Data.Maybe           (fromMaybe)
import Data.Data            (Typeable)

data Acid = Acid
   {
   acidUserListState    :: AcidState UserList
   }
