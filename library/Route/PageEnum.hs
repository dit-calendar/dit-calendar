{-# LANGUAGE TemplateHaskell, TypeOperators, DeriveDataTypeable, OverloadedStrings #-}

module Route.PageEnum ( Sitemap(..), urlSitemapParser ) where

import Prelude                 hiding ( (.) )

import Control.Category              ( (.) )
import Web.Routes.Boomerang          ( Router, int, lit, (:-), (</>), (<>) )
import Data.Data                     ( Data, Typeable )
import Text.Boomerang.TH             ( makeBoomerangs )

import Data.Domain.Types             ( UserId, EntryId, TaskId )


--A url type
data Sitemap
  = Home
 -- | Authenticate AuthenticateURL
  | Restricted
  | User UserId
  | Userdetail
  | CalendarEntry EntryId
  | Task TaskId
  | TaskWithCalendar TaskId EntryId
  | TaskWithUser TaskId UserId
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(makeBoomerangs ''Sitemap)

urlSitemapParser :: Router () (Sitemap :- ())
urlSitemapParser =
       rHome
    <> rCalendarEntry . (lit "calendarentry" </> int)
    <> lit "user" . userMapping
    <> lit "task" . taskMapping
    where
      userMapping = rUserdetail
            <> rUser </> int
      taskMapping = rTask </> int
            <> rTaskWithUser </> int </> lit "user" </> int
            <> rTaskWithCalendar </> int </> lit "calendarentry" </> int