{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.Domain.Task where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )
import Data.Domain.Types        ( UserId, TaskId )

data Task = Task {
    description      :: String
    , entryId        :: TaskId
    , belongingUsers :: [UserId]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Task)