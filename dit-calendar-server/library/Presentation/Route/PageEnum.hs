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
    --calendar mapping
    | CalendarEntry
    | CalendarEntryDetail EntryId
    -- tasks mapping
    | CalendarTask EntryId
    | CalendarTaskDetail EntryId TaskId
    | TaskWithUser EntryId TaskId
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(makeBoomerangs ''Sitemap)

urlSitemapParser :: Router () (Sitemap :- ())
urlSitemapParser =
       rHome
    <> rAuthenticate . (lit "authenticate" </> authenticateURL)
    <> lit "users" . userMapping
    <> lit "calendarentries" . calendarTaskMapping
    where
        userMapping = rUsers
            <> rUserdetail </> lit "me"
            <> rUser </> int
        calendarTaskMapping =
            calendarMapping
            <> taskMapping
        calendarMapping =
            rCalendarEntry --create, findAll
            <> rCalendarEntryDetail </> int -- read, update, delete
        taskMapping =
            rCalendarTask </> int </> lit "tasks" -- create, findAll
            <> rCalendarTaskDetail </> int </> lit "tasks" </> int -- read, update, delete
            <> rTaskWithUser </> int </> lit "tasks" </> int </> lit "assignment" -- add/remove user to Task
