{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.Domain.CalendarEntry where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )
import Data.Domain.Types             ( UserId, EntryId )

data CalendarEntry = CalendarEntry {
    description    :: String
    , entryId      :: EntryId
    , userId       :: UserId
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CalendarEntry)