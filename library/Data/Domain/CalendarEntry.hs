{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.Domain.CalendarEntry where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )
import Data.Domain.Types        ( UserId, EntryId, TaskId, Entry(..) )
import Data.Time.Clock          ( UTCTime )

data CalendarEntry = CalendarEntry {
    description      :: String
    , entryId        :: EntryId
    , userId         :: UserId
    , tasks          :: [TaskId]
    , date           :: UTCTime
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CalendarEntry)

instance Entry CalendarEntry where
    setId calendarEntry newId = calendarEntry { entryId = newId }
    getId = entryId