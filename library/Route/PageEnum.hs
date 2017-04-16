{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Route.PageEnum where

import Prelude                 hiding ( head )

import Data.Data                     ( Data, Typeable )
import Web.Routes.TH                 ( derivePathInfo )

import Domain.Types     			 ( UserId, EntryId )


--A url type
data SiteMap
  = Home
  | User UserId
  | Userdetail
  | CalendarEntry EntryId
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''SiteMap)
