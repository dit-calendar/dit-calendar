{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Domain.Calendar.CalendarEntry where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )

type EntryId = Int

data CalendarEntry = CalendarEntry { description :: String, entryId :: EntryId }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CalendarEntry)