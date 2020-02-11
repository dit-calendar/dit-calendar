{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.Domain.Task where

import           Data.Data         (Data, Typeable)
import           Data.Default
import           Data.Domain.Types (Description, Entity (..), TaskId, TelegramChatId)
import           Data.SafeCopy     (base, deriveSafeCopy)
import           Data.Time.Clock   (UTCTime)

data Task = Task {
    description      :: Description
    , taskId         :: TaskId
    , version        :: Int
    , assignedUsers  :: [TelegramChatId]
    , startTime      :: Maybe UTCTime
    , endTime        :: Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Task)

instance Entity Task where
    setId task newId = task { taskId = newId }
    getId = taskId
    getVersion = version
    setVersion version newVersion = version { version = newVersion}
    getUsersAccessRestriction _ = []

instance Default Task where
    def = Task {taskId = -1, version = 0, assignedUsers = []}