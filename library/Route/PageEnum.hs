{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Route.PageEnum where

import Prelude                hiding ( head )

import Data.Data                     ( Data, Typeable )
import Web.Routes.TH                 ( derivePathInfo )

import Data.Domain.Types             ( UserId, EntryId, TaskId )


--A url type
data SiteMap
  = Home
  | User UserId
  | Userdetail
  | CalendarEntry EntryId
  | Task TaskId
  | TaskWithCalendar EntryId UserId
  | TaskWithUser TaskId UserId
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''SiteMap)
