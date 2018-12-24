{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.Domain.User
    ( User(..)
    ) where

import           Data.Data         (Data, Typeable)
import           Data.Default
import           Data.SafeCopy     (base, deriveSafeCopy)
import           Data.Text         (Text)

import           Data.Domain.Types (Entry (..), EntryId, TaskId, UserId)

data User = User
    { loginName       :: Text
    , userId          :: UserId
    , version         :: Int
    , calendarEntries :: [EntryId]
    , belongingTasks  :: [TaskId]
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''User)

instance Entry User where
    setId user newId = user {userId = newId}
    getId = userId
    incVersion user = user { version  = version user + 1}
    getVersion = version

instance Default User where
    def = User {userId = -1, version = 0, calendarEntries = [], belongingTasks = []}
