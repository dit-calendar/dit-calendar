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

import           Data.Domain.Types (Description, Entry (..), EntryId, TaskId,
                                    UserId)

data CalendarEntry = CalendarEntry
    { description :: Description
    , entryId     :: EntryId
    , version     :: Int
    , userId      :: UserId
    , tasks       :: [TaskId]
    , date        :: UTCTime
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CalendarEntry)

instance Entry CalendarEntry where
    setId calendarEntry newId = calendarEntry {entryId = newId}
    getId = entryId
    incVersion calendarEntry = calendarEntry {version = version calendarEntry + 1}
    getVersion = version

instance Default CalendarEntry where
    def = CalendarEntry {entryId = -1, version = 0, tasks = []}
