{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Domain.CalendarEntry where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )
import Domain.Types             ( UserId, EntryId )

data CalendarEntry = CalendarEntry {
    description :: String
    , entryId :: EntryId
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CalendarEntry)