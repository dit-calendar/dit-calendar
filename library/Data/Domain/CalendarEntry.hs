{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data.Domain.CalendarEntry
    ( CalendarEntry(..)
    ) where

import           Data.Data         (Data, Typeable)
import           Data.Domain.Types (Description, Entry (..), EntryId, TaskId,
                                    UserId)
import           Data.SafeCopy     (base, deriveSafeCopy)
import           Data.Time.Clock   (UTCTime)

data CalendarEntry = CalendarEntry
    { description :: Description
    , entryId     :: EntryId
    , userId      :: UserId
    , tasks       :: [TaskId]
    , date        :: UTCTime
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CalendarEntry)

instance Entry CalendarEntry where
    setId calendarEntry newId = calendarEntry {entryId = newId}
    getId = entryId
