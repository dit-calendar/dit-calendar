{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Presentation.Route.PageEnum ( Sitemap(..), urlSitemapParser ) where

import           Prelude                     hiding ((.))

import           Control.Category            ((.))
import           Data.Data                   (Data, Typeable)
import           Happstack.Authenticate.Core (AuthenticateURL, authenticateURL)
import           Text.Boomerang.TH           (makeBoomerangs)
import           Web.Routes.Boomerang        ((:-), Router, anyText, int, lit,
                                              (</>), (<>))

import           Data.Domain.Types           (EntryId, TaskId, UserId)


--A url type
data Sitemap
    = Home
    | Authenticate AuthenticateURL
    | User UserId
    | Userdetail
    | Users
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
    <> rAuthenticate . (lit "authenticate" </> authenticateURL)
    <> lit "user" . userMapping
    <> lit "task" . taskMapping
    <> lit "users" . rUsers
    where
        userMapping = rUserdetail </> lit "me"
            <> rUser </> int
        taskMapping = rTask </> int
            <> rTaskWithUser </> int </> lit "user" </> int
            <> rTaskWithCalendar </> int </> lit "calendarentry" </> int
