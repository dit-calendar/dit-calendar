{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.Domain.Task where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )
import Data.Domain.Types        ( UserId, TaskId, Entry(..) )

data Task = Task {
    description      :: String
    , taskId         :: TaskId
    , belongingUsers :: [UserId]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Task)

instance Entry Task where
    setId task newId = task { taskId = newId }
    getId = taskId