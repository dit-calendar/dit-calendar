{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.Domain.CalendarEntry
    ( CalendarEntry(..)
    ) where

import           Data.Data         (Data, Typeable)
import           Data.Default
import           Data.SafeCopy     (base, deriveSafeCopy)
import           Data.Time.Clock   (UTCTime)

import           Data.Domain.Types (Description, Entity (..), EntryId, TaskId,
                                    UserId, StartDate, EndDate)

data CalendarEntry = CalendarEntry
    { description :: Description
    , entryId     :: EntryId
    , version     :: Int
    , owner       :: UserId
    , tasks       :: [TaskId]
    , startDate   :: StartDate
    , endDate     :: EndDate
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CalendarEntry)

instance Entity CalendarEntry where
    setId calendarEntry newId = calendarEntry {entryId = newId}
    getId = entryId
    setVersion calendarEntry newVersion = calendarEntry {version = newVersion}
    getVersion = version
    getUsersAccessRestriction a = [owner a]

instance Default CalendarEntry where
    def = CalendarEntry {entryId = -1, version = 0, tasks = [], owner = -1}
