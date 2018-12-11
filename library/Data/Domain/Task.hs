{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data.Domain.Task where

import           Data.Data         (Data, Typeable)
import           Data.Domain.Types (Description, Entry (..), TaskId, UserId)
import           Data.SafeCopy     (base, deriveSafeCopy)
import           Data.Time.Clock   (UTCTime)

data Task = Task {
    description      :: Description
    , taskId         :: TaskId
    , belongingUsers :: [UserId]
    , startTime      :: Maybe UTCTime
    , endTime        :: Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Task)

instance Entry Task where
    setId task newId = task { taskId = newId }
    getId = taskId
