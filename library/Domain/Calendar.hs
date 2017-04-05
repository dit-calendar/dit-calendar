{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Domain.Calendar where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )


data Calendar = Calendar { description :: String, calendarId :: Int }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''User)