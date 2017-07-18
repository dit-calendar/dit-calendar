{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.Domain.User where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )

import Data.Domain.Types             ( UserId, EntryId, TaskId )

data User = User { name :: String, userId :: UserId, calendarEntries :: [EntryId], belongingTasks :: [TaskId] }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''User)