{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.Domain.CalendarEntry
    ( CalendarEntry(..)
    ) where

import           Data.Data         (Data, Typeable)
import           Data.Default
import           Data.SafeCopy     (base, deriveSafeCopy)

import           Data.Domain.Types (Description, EndDate, Entity (..), EntryId,
                                    StartDate, TaskId, Title, UserId)

data CalendarEntry = CalendarEntry
    { title       :: Title
    , description :: Maybe Description
    , entryId     :: EntryId
    , version     :: Int
    , owner       :: UserId
    , tasks       :: [TaskId]
    , startDate   :: StartDate
    , endDate     :: EndDate
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CalendarEntry)

instance Entity CalendarEntry EntryId where
    setId calendarEntry newId = calendarEntry {entryId = newId}
    getId = entryId
    setVersion calendarEntry newVersion = calendarEntry {version = newVersion}
    getVersion = version
    getUsersAccessRestriction a = [owner a]

instance Default CalendarEntry where
    def = CalendarEntry {entryId = -1, version = 0, tasks = [], owner = -1}
