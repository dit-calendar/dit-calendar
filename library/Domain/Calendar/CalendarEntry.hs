{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Domain.Calendar.CalendarEntry where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )


data CalendarEntry = CalendarEntry { description :: String, entryId :: Int }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CalendarEntry)