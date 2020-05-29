{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.Domain.Task where

import           Data.Data         (Data, Typeable)
import           Data.Default
import           Data.Domain.Types (Description, Entity (..), TaskId,
                                    TelegramChatId, Title, UserId)
import           Data.SafeCopy     (base, deriveSafeCopy)
import           Data.Time.Clock   (UTCTime)

data Task = Task {
    title                   :: Title
    , description           :: Maybe Description
    , taskId                :: TaskId
    , version               :: Int
    , owner                 :: UserId
    , assignedTelegramLinks :: [TelegramChatId]
    , startTime             :: Maybe UTCTime
    , endTime               :: Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Task)

instance Entity Task TaskId where
    setId task newId = task { taskId = newId }
    getId = taskId
    getVersion = version
    setVersion version newVersion = version { version = newVersion}
    getUsersAccessRestriction a = [owner a]

instance Default Task where
    def = Task {taskId = -1, version = 0, assignedTelegramLinks = []}
