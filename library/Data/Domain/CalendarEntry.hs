{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.Domain.CalendarEntry where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )
import Data.Domain.Types        ( UserId, EntryId, TaskId, Entry(..) )

data CalendarEntry = CalendarEntry {
    description      :: String
    , entryId        :: EntryId
    , userId         :: UserId
    , tasks          :: [TaskId]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CalendarEntry)

instance Entry CalendarEntry where
    setId calendarEntry newId = calendarEntry { entryId = newId }
    getId = entryId